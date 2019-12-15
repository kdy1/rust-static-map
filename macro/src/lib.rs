extern crate proc_macro;

use self::util::ItemImplExt;
use pmutil::{prelude::*, smart_quote};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse, punctuated::Punctuated, Data, DeriveInput, Expr, FieldValue, Fields, GenericParam,
    ItemImpl, Token,
};

mod util;

enum Mode {
    ByValue,
    ByRef,
    ByMutRef,
}

#[proc_macro_derive(StaticMap)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::<DeriveInput>(input).expect("failed to parse input as DeriveInput");
    let name = input.ident.clone();

    let fields = match input.data {
        Data::Struct(s) => {
            if s.fields.is_empty() {
                panic!("StaticMap: failed to detect type because there's no field")
            }

            match s.fields {
                Fields::Named(named) => named.named,
                _ => panic!("StaticMap: failed to detect type because there's no field"),
            }
        }
        _ => panic!("StaticMap can only be applied to structs"),
    };
    let (data_type, len) = (fields.first().unwrap().ty.clone(), fields.len());

    let mut tts = TokenStream::new();

    {
        // Iterators

        let make = |m: Mode| {
            let arr: Punctuated<_, Token![;]> = fields
                .iter()
                .map(|f| {
                    //
                    Quote::new_call_site()
                        .quote_with(smart_quote!(
                            Vars {
                                name: f.ident.as_ref().unwrap(),

                                mode: match m {
                                    Mode::ByValue => quote!(),
                                    Mode::ByRef => quote!(&),
                                    Mode::ByMutRef => quote!(&mut),
                                },

                                // self.field
                                value: f.ident.as_ref().unwrap(),
                            },
                            (v.push((stringify!(name), mode self.value)))
                        ))
                        .parse::<Expr>()
                })
                .collect();

            arr
        };

        Quote::new_call_site()
            .quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    len,
                    iter_body: make(Mode::ByRef),
                    iter_mut_body: make(Mode::ByMutRef),
                },
                {
                    impl Type {
                        fn iter(&self) -> impl Iterator<Item = (&'static str, &T)> {
                            let mut v: static_map::arrayvec::ArrayVec<[_; len]> =
                                Default::default();

                            iter_body;

                            v.into_iter()
                        }

                        fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut T)> {
                            let mut v: static_map::arrayvec::ArrayVec<[_; len]> =
                                Default::default();

                            iter_mut_body;

                            v.into_iter()
                        }
                    }
                }
            ))
            .parse::<ItemImpl>()
            .with_generics(input.generics.clone())
            .to_tokens(&mut tts);

        Quote::new_call_site()
            .quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    body: make(Mode::ByValue),
                    len
                },
                {
                    impl IntoIterator for Type {
                        type Item = (&'static str, T);
                        type IntoIter = static_map::arrayvec::IntoIter<[(&'static str, T); len]>;

                        fn into_iter(self) -> Self::IntoIter {
                            let mut v: static_map::arrayvec::ArrayVec<[_; len]> =
                                Default::default();

                            body;

                            v.into_iter()
                        }
                    }
                }
            ))
            .parse::<ItemImpl>()
            .with_generics(input.generics.clone())
            .to_tokens(&mut tts);
    }

    {
        assert!(
            input.generics.params.len() == 0 || input.generics.params.len() == 1,
            "StaticMap should have zero or one generic argument"
        );

        let map_fields: Punctuated<_, Token![,]> = fields
            .iter()
            .map(|f| {
                Quote::new_call_site()
                    .quote_with(smart_quote!(
                        Vars {
                            f: f.ident.as_ref().unwrap()
                        },
                        (f: op(stringify!(f), self.f))
                    ))
                    .parse::<FieldValue>()
            })
            .collect();

        // map(), map_value()
        let item = if input.generics.params.is_empty() {
            Quote::new_call_site().quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    fields: &map_fields,
                },
                {
                    impl Type {
                        pub fn map(self, mut op: impl FnMut(&'static str, T) -> T) -> Type {
                            Type { fields }
                        }

                        #[inline]
                        pub fn map_value(self, mut op: impl FnMut(T) -> T) -> Type {
                            self.map(|_, v| op(v))
                        }
                    }
                }
            ))
        } else if match input.generics.params.first().as_ref().unwrap() {
            GenericParam::Type(ty) => ty.bounds.is_empty(),
            _ => false,
        } {
            Quote::new_call_site().quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    fields: &map_fields,
                },
                {
                    impl<T> Type<T> {
                        pub fn map<N>(self, mut op: impl FnMut(&'static str, T) -> N) -> Type<N> {
                            Type { fields }
                        }

                        #[inline]
                        pub fn map_value<N>(self, mut op: impl FnMut(T) -> N) -> Type<N> {
                            self.map(|_, v| op(v))
                        }
                    }
                }
            ))
        } else {
            let bound = match input.generics.params.first().as_ref().unwrap() {
                GenericParam::Type(ty) => &ty.bounds,
                _ => unimplemented!("Generic parameters other than type parameter"),
            };

            Quote::new_call_site().quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    fields: &map_fields,
                    Bound: &bound,
                },
                {
                    impl<T: Bound> Type<T> {
                        pub fn map<N: Bound>(
                            self,
                            mut op: impl FnMut(&'static str, T) -> N,
                        ) -> Type<N> {
                            Type { fields }
                        }

                        #[inline]
                        pub fn map_value<N: Bound>(self, mut op: impl FnMut(T) -> N) -> Type<N> {
                            self.map(|_, v| op(v))
                        }
                    }
                }
            ))
        };

        item.to_tokens(&mut tts);
    }

    tts.into()
}
