use super::*;
use location::Located;
use semantic_parser::semantic_ast::{Binding, CallExpression, Expression};

pub(crate) fn analyze_expression(errors: Errors, context: Context, expr: &Located<Expression>) {
    match &expr.value {
        Expression::Let(let_expr) => {
            variable_scope!(context, {
                for (binding, annotation, value) in &let_expr.bindings {
                    let value_ty = infer_expression_type(errors, context.clone(), &value);
                    let annotated_ty = if let Some(annotation_ty) = annotation {
                        context.borrow().resolve_type(&annotation_ty).clone()
                    } else {
                        ResolvedType::Unknown
                    };
                    if !value_ty.is_assinable_to(&annotated_ty, context.clone()) {
                        errors.push(AnalyzeError {
                            loc: value.range,
                            message: format!("{} is not assignable to {}", value_ty, annotated_ty),
                        });
                    }
                    match &binding.value {
                        Binding::Simple(name) => {
                            context
                                .borrow_mut()
                                .variable_scopes
                                .last_mut()
                                .unwrap()
                                .insert(
                                    name.clone(),
                                    if !annotated_ty.is_unknown() {
                                        annotated_ty.clone()
                                    } else {
                                        value_ty.clone()
                                    },
                                );
                        }
                        Binding::Complex { keys: _, alias: _ } => todo!(),
                    };
                }

                for expr in &let_expr.body {
                    analyze_expression(errors, context.clone(), expr)
                }
            });
        }
        Expression::Call(call_expr) => {
            let fn_type = infer_expression_type(errors, context.clone(), &call_expr.func_expr);
            if let ResolvedType::Fn {
                return_ty: _,
                arg_types,
            } = fn_type
            {
                for (i, arg) in call_expr.args.iter().enumerate() {
                    if let Some(argument_ty) = arg_types.get(i) {
                        let value_ty = infer_expression_type(errors, context.clone(), &arg);
                        if !value_ty.is_assinable_to(argument_ty, context.clone()) {
                            errors.push(AnalyzeError {
                                loc: expr.range,
                                message: format!(
                                    "{} is not assignable to {}",
                                    value_ty, argument_ty,
                                ),
                            })
                        }
                    }
                }
            } else {
                errors.push(AnalyzeError {
                    loc: expr.range,
                    message: format!("Trying to call not function value"),
                })
            }
        }
        _ => return,
    }
}
