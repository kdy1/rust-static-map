extern crate proc_macro;

use self::util::ItemImplExt;
use pmutil::{prelude::*, smart_quote};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::iter::once;
use syn::{
    parse, parse_quote, punctuated::Punctuated, token::Comma, Arm, Data, DeriveInput, Expr,
    ExprLit, ExprMatch, Field, FieldValue, Fields, GenericParam, Generics, Ident, Item, ItemImpl,
    ItemStruct, Lit, LitStr, Pat, PatLit, Token, Type,
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

        let mut items = vec![];
        items.extend(make_iterator(
            &name,
            &data_type,
            &Ident::new(&format!("{name}Iter"), Span::call_site()),
            &fields,
            Mode::ByValue,
        ));
        items.extend(make_iterator(
            &name,
            &data_type,
            &Ident::new(&format!("{name}RefIter"), Span::call_site()),
            &fields,
            Mode::ByRef,
        ));
        items.extend(make_iterator(
            &name,
            &data_type,
            &Ident::new(&format!("{name}MutIter"), Span::call_site()),
            &fields,
            Mode::ByMutRef,
        ));

        for item in items {
            item.to_tokens(&mut tts);
        }
    }

    {
        // std::ops::Index
        let body = ExprMatch {
            attrs: Default::default(),
            match_token: Default::default(),
            expr: Quote::new_call_site()
                .quote_with(smart_quote!(Vars {}, { v }))
                .parse(),
            brace_token: Default::default(),
            arms: fields
                .iter()
                .map(|f| {
                    //
                    Arm {
                        attrs: Default::default(),
                        pat: Pat::Lit(PatLit {
                            attrs: Default::default(),
                            expr: Box::new(Expr::Lit(ExprLit {
                                attrs: Default::default(),
                                lit: Lit::Str(LitStr::new(
                                    &f.ident.as_ref().unwrap().to_string(),
                                    Span::call_site(),
                                )),
                            })),
                        }),
                        guard: None,
                        fat_arrow_token: Default::default(),
                        body: Quote::new_call_site()
                            .quote_with(smart_quote!(Vars { variant: &f.ident }, { &self.variant }))
                            .parse(),
                        comma: Some(Default::default()),
                    }
                })
                .chain(once(
                    Quote::new_call_site()
                        .quote_with(smart_quote!(Vars {}, {
                            _ => panic!("Unknown key: {}", v),
                        }))
                        .parse(),
                ))
                .collect(),
        };

        Quote::new_call_site()
            .quote_with(smart_quote!(
                Vars {
                    Type: &name,
                    T: &data_type,
                    body,
                },
                {
                    impl<'a, K: ?Sized + ::std::borrow::Borrow<str>> ::std::ops::Index<&'a K> for Type {
                        type Output = T;
                        fn index(&self, v: &K) -> &Self::Output {
                            use std::borrow::Borrow;
                            let v: &str = v.borrow();
                            body
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

fn make_iterator(
    type_name: &Ident,
    data_type: &Type,
    iter_type_name: &Ident,
    fields: &Punctuated<Field, Comma>,
    mode: Mode,
) -> Vec<Item> {
    let item_type = match mode {
        Mode::ByValue => quote!(#data_type),
        Mode::ByRef => quote!(&#data_type),
        Mode::ByMutRef => quote!(&mut #data_type),
    };

    let generic = match mode {
        Mode::ByValue => quote!(),
        Mode::ByRef => quote!(<'a>),
        Mode::ByMutRef => quote!(<'a>),
    };

    let lifetime = match mode {
        Mode::ByValue => quote!(),
        Mode::ByRef => quote!(&'a),
        Mode::ByMutRef => quote!(&'a mut),
    };

    let iter_type = parse_quote!(
        struct #iter_type_name #generic {
            cur_index: usize,
            data: #lifetime #type_name<#data_type>,
        }
    );
    let iter_impl = parse_quote!(
        impl #generic Iterator for #iter_type_name #generic {
            type Item = (&'static str, #lifetime #item_type);
        }
    );

    vec![iter_type, Item::Impl(iter_impl)]
}
