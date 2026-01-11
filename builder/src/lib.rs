use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, DataStruct, DeriveInput, Fields,
    FieldsNamed, Ident, Meta, MetaList, Path, PathArguments, Type, TypePath,
};

struct Field {
    /// name of the field
    name: Ident,
    /// type of the field
    ty: Type,
    is_optional: bool,
    /// value of the each attr coming from:
    /// #[builder(each = "value")]
    each_attr_value: Option<Ident>,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{ident}Builder"), proc_macro2::Span::call_site());

    let syn::Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed {
            named: field_names, ..
        }),
        ..
    }) = input.data
    else {
        panic!("expected a struct");
    };
    let mut fields = vec![];
    for f in field_names {
        let name = f.ident.unwrap();
        let ty = f.ty;

        let Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) = ty.clone()
        else {
            panic!("invalid");
        };

        fields.push(Field {
            name,
            ty,
            is_optional: segments[0].clone().ident == "Option",
            each_attr_value: match parse_builder_each_attr(&f.attrs) {
                Ok(a) => a,
                Err(e) => return e.into_compile_error().into(),
            },
        })
    }

    let OutputParts {
        builder_inner_type_defs,
        builder_initializer_sets,
        setter_functions,
        each_attr_functions,
        build_function_setters,
        ..
    } = create_output_parts(&fields);

    let q = quote! {
        pub struct #builder_ident {
            #(#builder_inner_type_defs),*
        }

        impl #builder_ident {
            pub fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::core::error::Error>> {
                Ok(#ident {
                    #(#build_function_setters),*
                })
            }

            #(#setter_functions)*

            #(#each_attr_functions)*
        }
        //
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_initializer_sets),*
                }
            }
        }
    };

    q.into()
}

struct OutputParts {
    // inside of `struct NBuilder {}`
    builder_inner_type_defs: Vec<TokenStream>,

    // the initialization pairs when initializing the `NBuilder`. eg `typ: None`
    builder_initializer_sets: Vec<TokenStream>,

    // the setter functions in the builder. eg `fn field_name(&mut self) -> &mut Self { .. }`
    setter_functions: Vec<TokenStream>,

    // the functions created from the `each` attribute
    each_attr_functions: Vec<TokenStream>,

    // setters `key: value` within the `build` function
    build_function_setters: Vec<TokenStream>,
}

fn create_builder_fields_definition(f: &Field) -> TokenStream {
    let name = &f.name;
    let ty = &f.ty;
    if f.each_attr_value.is_some() || f.is_optional {
        quote! {
            #name: #ty
        }
    } else {
        quote! {
            #name: ::std::option::Option<#ty>
        }
    }
}

fn create_builder_initializer(f: &Field) -> TokenStream {
    let name = &f.name;
    if f.each_attr_value.is_some() {
        quote! {
            #name: vec![]
        }
    } else {
        quote! {
            #name: None
        }
    }
}

fn create_setter_function(f: &Field) -> TokenStream {
    let name = &f.name;
    let ty = &f.ty;
    let setter = |ty| {
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = ::std::option::Option::Some(#name);
                self
            }
        }
    };

    if let Some(value) = &f.each_attr_value {
        // the field and the each attr value is the same, which means we don't need to
        // create a setter function
        if value == &f.name {
            quote! {}
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        }
    } else {
        if f.is_optional {
            let Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) = ty
            else {
                panic!("invalid optional field ");
            };
            let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &segments[0].arguments
            else {
                panic!("unexpected type");
            };

            setter(args.to_token_stream())
        } else {
            setter(ty.to_token_stream())
        }
    }
}

fn create_each_attr_function(f: &Field) -> TokenStream {
    if let Some(value) = &f.each_attr_value {
        let name = &f.name;
        let ty = &f.ty;
        let Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) = ty
        else {
            panic!("invalid optional field ");
        };
        let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            &segments[0].arguments
        else {
            panic!("unexpected type");
        };
        quote! {
            pub fn #value(&mut self, item: #args) -> &mut Self {
                self.#name.push(item);
                self
            }
        }
    } else {
        quote! {}
    }
}

fn create_build_function_setter(f: &Field) -> TokenStream {
    let name = &f.name;
    if f.is_optional || f.each_attr_value.is_some() {
        quote! {
            #name: self.#name.clone()
        }
    } else {
        quote! {
            #name: self.#name.clone().expect("mandatory field is not set")
        }
    }
}

fn create_output_parts(fields: &[Field]) -> OutputParts {
    let mut output = OutputParts {
        builder_inner_type_defs: vec![],
        builder_initializer_sets: vec![],
        setter_functions: vec![],
        each_attr_functions: vec![],
        build_function_setters: vec![],
    };

    fields.iter().for_each(|f| {
        output
            .builder_inner_type_defs
            .push(create_builder_fields_definition(f));
        output
            .builder_initializer_sets
            .push(create_builder_initializer(f));
        output.setter_functions.push(create_setter_function(f));
        output
            .each_attr_functions
            .push(create_each_attr_function(f));
        output
            .build_function_setters
            .push(create_build_function_setter(f));
    });

    output
}

/// Parse arguments like:
/// #[builder(each = "arg")]
fn parse_builder_each_attr(attributes: &[Attribute]) -> Result<Option<Ident>, syn::Error> {
    for a in attributes {
        let Meta::List(MetaList { path, tokens, .. }) = &a.meta else {
            continue;
        };

        if path.segments.is_empty() {
            return Err(syn::Error::new(
                Span::call_site(),
                "unknown attribute: empty segment",
            ));
        }

        if path.segments[0].ident != "builder" {
            return Err(syn::Error::new(Span::call_site(), "unknown attribute"));
        }

        let token_list: Vec<_> = tokens.clone().into_iter().collect();

        let TokenTree::Ident(inner) = &token_list.get(0).expect("expected a token") else {
            return Err(syn::Error::new(
                Span::call_site(),
                "attribute builder needs be in format #[builder(ident = \"argument\")]",
            ));
        };

        if inner != "each" {
            return Err(syn::Error::new(
                inner.span(),
                "expected `builder(each = \"...\")`",
            ));
        }

        let TokenTree::Literal(literal) = &token_list.get(2).expect("expected a token") else {
            return Err(syn::Error::new(
                Span::call_site(),
                "attribute builder needs be in format #[builder(ident = \"argument\")]",
            ));
        };

        return Ok(Some(Ident::new(
            literal.clone().to_string().trim_matches('"'),
            Span::call_site(),
        )));
    }

    Ok(None)
}
