module astdump;

// TODO: remove unnec. imports
import std.stdio, std.path, std.array, std.string, std.algorithm, std.conv;
import std.exception, std.json;
import file=std.file;
import util;
import ast.lexer, ast.parser, ast.expression, ast.declaration, ast.error, ast.type, help;
import astopt;
import options, ast.scope_, ast.semantic_, ast.summarize;

string jsonObj(string name, string props){
    return "{\n"~jsonProp("expType", "\""~name~"\"")~props~"}";
}

string jsonProp(string name, string prop){
    return "\""~name~"\": "~prop~",\n";
}

string jsonProp(string name, bool prop){
	if (prop){
		return "\""~name~"\": true,\n";
	}
	return "\""~name~"\": false,\n";
}

string lrHandSide(string lhs, string rhs){
    return jsonProp("lhs", lhs)~jsonProp("rhs", rhs);
}

string operation(string op, string arg){
    return jsonProp("op", op)~jsonProp("arg", arg);
}

struct ASTDumper{
    FunctionDef[string] functions;
    this(FunctionDef[string] functions){
        this.functions = functions;
    }

    void dumpAST(){
        foreach (name, ops; this.functions){
            foreach (stmt; ops.body_.s){
                auto s = dumpStm(stmt);
				writeln(s);
				parseJSON(s);
            }
        }
    }

    string dumpStm(Expression e){
		// TODO: Error - try and catch
		return dumpStm2(e);
	}
	string dumpStm2(Expression e){
	// 	if(opt.trace && !isInPrelude(functionDef)){
	// 		writeln(qstate);
	// 		writeln();
	// 		writeln("STATEMENT");
	// 		writeln(e);
	// 		writeln();
	// 	}
		if(auto nde=cast(DefExp)e){
            auto de=cast(DefineExp)nde.initializer;
            return dumpStm2(de);
		}else if(auto ae=cast(AssignExp)e){
            auto lhs=dumpExp(ae.e1),rhs=dumpExp(ae.e2);
            return jsonObj("assignExp", lrHandSide(lhs, rhs));
        }else if(auto ae=cast(DefineExp)e){
            if(ae.isSwap){
                // TODO
                return "";
            }else{
        		auto lhs=dumpExp(ae.e1),rhs=dumpExp(ae.e2);
            	return jsonObj("defineExp", lrHandSide(lhs, rhs));
            }
		}
	// else if(auto ce=cast(CatAssignExp)e){
	// 	}else if(isOpAssignExp(e)){
	// 		QState.Value perform(QState.Value a,QState.Value b){
	// 			if(cast(OrAssignExp)e) ;
	// 			if(cast(AndAssignExp)e) ;
	// 			if(cast(AddAssignExp)e) ;
	// 			if(cast(SubAssignExp)e) ;
	// 			if(cast(MulAssignExp)e) ;
	// 			if(cast(DivAssignExp)e||cast(IDivAssignExp)e){}
	// 			if(cast(ModAssignExp)e) ;
	// 			if(cast(PowAssignExp)e){}
	// 			if(cast(BitOrAssignExp)e) ;
	// 			if(cast(BitXorAssignExp)e) ;
	// 			if(cast(BitAndAssignExp)e) ;
	// 			assert(0);
	// 		}
	//  }else if(auto call=cast(CallExp)e){
	// 	}else if(auto ce=cast(CompoundExp)e){
	// 	}else if(auto ite=cast(IteExp)e){
	// 	}else if(auto re=cast(RepeatExp)e){
	// 	}else if(auto fe=cast(ForExp)e){
	// 	}else if(auto we=cast(WhileExp)e){
		// }
		else if(auto re=cast(ReturnExp)e){
            auto value = dumpExp(re.e);
            return jsonObj("returnExp", value);
		}
	// 	}else if(auto ae=cast(AssertExp)e){
	//  }else if(auto oe=cast(ObserveExp)e){ // ignore (not implemented in Silq)
	//  }else if(auto fe=cast(ForgetExp)e){
	// 	}else if(auto ce=cast(CommaExp)e){
	// 	}else if(auto fd=cast(FunctionDef)e){
	// 	}else if(cast(Declaration)e){
	// 		// do nothing
		// }
		else{
			enforce(0,text("StmtTODO: ",e));
		}
        assert(0);
	}


	string dumpExp(Expression e){
	// 	if(!qstate.state.length) return QState.Value.init;
		string doIt()(Expression e){
			// TODO: errors - try, catch
			return doIt2(e);
		}
	// 	// TODO: get rid of code duplication
		string doIt2(Expression e){
	// 		if(e.type == typeTy) return QState.typeValue; // TODO: get rid of this
			if(auto id=cast(Identifier)e){
                if(!id.meaning&&util.among(id.name,"π","pi")) return "pi";
				if(id.substitute){
					if(auto vd=cast(VarDecl)id.meaning){
						return jsonObj("varDecl", doIt2(vd.initializer));
					}
				}
				// This changes qstate which affects CallExp cases
				// auto r=lookupMeaning(qstate,id);
				// enforce(r.isValid,"unsupported");
                // TODO: Check
                return jsonObj("varDecl", jsonProp("value", "\""~id.toString~"\""));
			}
			// if(auto fe=cast(FieldExp)e){
			// 	enforce(fe.type.isClassical||fe.constLookup);
			// 	if(isBuiltIn(fe)){
			// 		if(auto at=cast(ArrayTy)fe.e.type){
			// 			assert(fe.f.name=="length");
			// 			writeln("bin-Field");
			// 			doIt(fe.e);
			// 			enforce(r.tag==QState.Value.Tag.array_);
			// 			return qstate.makeInteger(ℤ(r.array_.length));
			// 		}
			// 	}
			// 	return qstate.readField(doIt(fe.e),fe.f.name,true);
			// }
	// 		if(auto ae=cast(AddExp)e) return doIt(ae.e1)+doIt(ae.e2);
	// 		if(auto me=cast(SubExp)e) return doIt(me.e1)-doIt(me.e2);
	// 		if(auto me=cast(NSubExp)e){
	// 			auto a=doIt(me.e1),b=doIt(me.e2);
	// 			enforce(a.ge(b).bval,"result of sub is negative");
	// 			return a-b;
	// 		}if(auto me=cast(MulExp)e) return doIt(me.e1)*doIt(me.e2);
	// 		if(auto de=cast(DivExp)e) return doIt(de.e1)/doIt(de.e2);
	// 		if(auto de=cast(IDivExp)e) return doIt(de.e1).opBinary!"div"(doIt(de.e2));
	// 		if(auto me=cast(ModExp)e) return doIt(me.e1)%doIt(me.e2);
	// 		if(auto pe=cast(PowExp)e) return doIt(pe.e1)^^doIt(pe.e2);
	// 		if(auto ce=cast(CatExp)e) return doIt(ce.e1)~doIt(ce.e2);
	// 		if(auto ce=cast(BitOrExp)e) return doIt(ce.e1)|doIt(ce.e2);
	// 		if(auto ce=cast(BitXorExp)e) return doIt(ce.e1)^doIt(ce.e2);
	// 		if(auto ce=cast(BitAndExp)e) return doIt(ce.e1)&doIt(ce.e2);
	// 		if(auto ume=cast(UMinusExp)e) return -doIt(ume.e);
	// 		if(auto ume=cast(UNotExp)e) return doIt(ume.e).eqZ;
	// 		if(auto ume=cast(UBitNotExp)e) return ~doIt(ume.e);
	// 		if(auto le=cast(LambdaExp)e) return qstate.makeFunction(le.fd);
			if(auto ce=cast(CallExp)e){
				auto id=cast(Identifier)unwrap(ce.e);
				auto fe=cast(FieldExp)unwrap(ce.e);
				auto fun=doIt(ce.e), arg=doIt(ce.arg);
				return jsonObj("callExp", operation(fun, arg));
			}
	// 		if(auto fe=cast(ForgetExp)e){
	// 		}
	// 		if(auto idx=cast(IndexExp)e){
	// 		}
	// 		if(auto sl=cast(SliceExp)e){
	// 		}
			if(auto le=cast(LiteralExp)e){
                return jsonObj("litExp", jsonProp("value", le.toString));
			}
	// 		if(auto ite=cast(IteExp)e){
			// }
            else if(auto tpl=cast(TupleExp)e){
				auto values="["~tpl.e.map!(e=>doIt(e)).fold!((a,b)=>a~",\n"~b)~"]";
				return jsonProp("value", values);
			}
            // else if(auto arr=cast(ArrayExp)e){
	// 		}else if(auto ae=cast(AssertExp)e){
			else if(auto tae=cast(TypeAnnotationExp)e){
				if(tae.e.type==tae.type) return doIt(tae.e);
				bool consume=!tae.constLookup; writeln(consume);
				auto expr = doIt(tae.e);
                auto props = jsonProp("expr", expr)~jsonProp("type", "\""~tae.type.toString~"\"")~jsonProp("consume", consume);
                // if(tae.constLookup) r=r.consumeOnRead();
                return jsonObj("typeChangeExp", props);
			}
	// 		else if(cast(Type)e)
	// 		else{
	// 			enum common=q{
	// 				auto e1=doIt(b.e1),e2=doIt(b.e2);
	// 			};
	// 			if(auto b=cast(AndExp)e){
	// 				mixin(common);
	// 				return e1&e2;
	// 			}else if(auto b=cast(OrExp)e){
	// 				mixin(common);
	// 				return e1|e2;
	// 			}else if(auto b=cast(LtExp)e){
	// 				mixin(common);
	// 				return e1.lt(e2);
	// 			}else if(auto b=cast(LeExp)e){
	// 				mixin(common);
	// 				return e1.le(e2);
	// 			}else if(auto b=cast(GtExp)e){
	// 				mixin(common);
	// 				return e1.gt(e2);
	// 			}else if(auto b=cast(GeExp)e){
	// 				mixin(common);
	// 				return e1.ge(e2);
	// 			}else if(auto b=cast(EqExp)e){
	// 				mixin(common);
	// 				return e1.eq(e2);
	// 			}else if(auto b=cast(NeqExp)e){
	// 				mixin(common);
	// 				return e1.neq(e2);
	// 			}
	// 		}
			enforce(0,text("ExpTODO: ",e," ",e.type));
			assert(0);
		}
		return doIt(e);
	}




}