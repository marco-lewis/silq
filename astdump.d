module astdump;

// TODO: remove unnec. imports
import std.stdio, std.path, std.array, std.string, std.algorithm, std.conv;
import std.exception, std.json;
import file=std.file;
import util;
import ast.lexer, ast.parser, ast.expression, ast.declaration, ast.error, ast.type, help;
import astopt;
import options, ast.scope_, ast.semantic_, ast.summarize;

string expObj(string name, string props){
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

string binExp(string name, string left, string right){
	return expObj(name, jsonProp("left", left)~jsonProp("right", right));
}

string unExp(string name, string val){
	return expObj(name, jsonProp("value", val));
}

struct ASTDumper{
    FunctionDef[string] functions;
	string fname;

    this(FunctionDef[string] functions, string fname){
        this.functions = functions;
		this.fname = fname;
    }

    void dumpAST(){
		auto funcJSON = "[\n";
        foreach (name, ops; this.functions){
			auto str = "[\n";
            foreach (stmt; ops.body_.s){
                str ~= dumpStm(stmt)~",\n";
            }
			str ~= "]";
			auto jv = parseJSON(str);
			funcJSON ~= "{\n"~jsonProp("func", "\""~name~"\"")~jsonProp("statements", str)~"},";
        }
		funcJSON ~= "]";
		auto jv = parseJSON(funcJSON);
		import std.file: write;
		write(this.fname, funcJSON);
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
            return expObj("assignExp", lrHandSide(lhs, rhs));
        }else if(auto ae=cast(DefineExp)e){
            if(ae.isSwap){
                // TODO
                return "";
            }else{
        		auto lhs=dumpExp(ae.e1),rhs=dumpExp(ae.e2);
            	return expObj("defineExp", lrHandSide(lhs, rhs));
            }
		}
		else if(auto ce=cast(CatAssignExp)e){
			auto lhs=dumpExp(ce.e1),rhs=dumpExp(ce.e2);
			return expObj("catAssignExp", lrHandSide(lhs, rhs));
		}
		// else if(isOpAssignExp(e)){
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
			// }
		else if(auto call=cast(CallExp)e){
			return dumpExp(call);
		}else if(auto ce=cast(CompoundExp)e){
			auto str = "[\n";
			foreach(s;ce.s) str ~= dumpStm(s)~",\n";
			str ~= "]";
			return expObj("compoundExp", jsonProp("statements", str));
		}else if(auto ite=cast(IteExp)e){
			auto cond=dumpExp(ite.cond);
			auto then=dumpStm(ite.then);
			auto othw=dumpStm(ite.othw);
			return expObj("iteExp", jsonProp("cond", cond)~jsonProp("then", then)~jsonProp("othw", othw));
		}else if(auto re=cast(RepeatExp)e){
			auto rep=dumpExp(re.num);
			auto bdy=dumpStm(re.bdy);
			return expObj("repeatExp", jsonProp("repeat", rep)~jsonProp("body", bdy));
		}else if(auto fe=cast(ForExp)e){
			auto l=dumpExp(fe.left), r=dumpExp(fe.right), s=fe.step?dumpExp(fe.step):"1", bdy=dumpStm(fe.bdy);
			return expObj("forExp", jsonProp("left",l)~jsonProp("right",r)~jsonProp("step",s)~jsonProp("body",bdy));
		}else if(auto we=cast(WhileExp)e){
			auto bdy=dumpStm(we.bdy);
			auto cond=dumpExp(we.cond);
			return expObj("whileExp", jsonProp("cond",cond)~jsonProp("body", bdy));
		}else if(auto re=cast(ReturnExp)e){
            auto value = dumpExp(re.e);
            return expObj("returnExp", value);
		}else if(auto ae=cast(AssertExp)e){
			return expObj("assertExp", jsonProp("cond", dumpExp(ae.e)));
	 	}else if(auto oe=cast(ObserveExp)e){ // ignore (not implemented as of current version Silq)
		}else if(auto fe=cast(ForgetExp)e){
			auto prop = jsonProp("var", dumpExp(fe.var));
			if(fe.val) prop~=jsonProp("val", dumpExp(fe.val));
			return expObj("forgetExp", prop);
		}else if(auto ce=cast(CommaExp)e){
			return dumpStm(ce.e1)~",\n"~dumpStm(ce.e2);
		}else if(auto fd=cast(FunctionDef)e){
			writeln("funcdef");
		}else if(cast(Declaration)e){
			// do nothing
		}
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
						return expObj("varDecl", doIt2(vd.initializer));
					}
				}
				// This changes qstate which affects CallExp cases
				// auto r=lookupMeaning(qstate,id);
				// enforce(r.isValid,"unsupported");
                // TODO: Check
                return "\""~id.toString~"\"";
			}
			if(auto fe=cast(FieldExp)e){
				writeln("field");
			}if(auto ae=cast(AddExp)e) return binExp("addExp", doIt(ae.e1), doIt(ae.e2));
			if(auto me=cast(SubExp)e) return binExp("subExp", doIt(me.e1), doIt(me.e2));
			if(auto me=cast(NSubExp)e) return binExp("nSubExp", doIt(me.e1), doIt(me.e2));
			if(auto me=cast(MulExp)e)return binExp("mulExp", doIt(me.e1), doIt(me.e2));
			if(auto de=cast(DivExp)e) return binExp("divExp", doIt(de.e1), doIt(de.e2));
			if(auto de=cast(IDivExp)e)return binExp("idivExp", doIt(de.e1), doIt(de.e2));
			if(auto me=cast(ModExp)e) return binExp("modExp", doIt(me.e1), doIt(me.e2));
			if(auto pe=cast(PowExp)e) return binExp("powExp", doIt(pe.e1), doIt(pe.e2));
			if(auto ce=cast(CatExp)e) return binExp("catExp", doIt(ce.e1), doIt(ce.e2));
			if(auto ce=cast(BitOrExp)e) return binExp("bitOrExp", doIt(ce.e1), doIt(ce.e2));
			if(auto ce=cast(BitXorExp)e) return binExp("bitXorExp", doIt(ce.e1), doIt(ce.e2));
			if(auto ce=cast(BitAndExp)e) return binExp("bitAndExp", doIt(ce.e1), doIt(ce.e2));
			if(auto ume=cast(UMinusExp)e) return unExp("uMinusExp", doIt(ume.e));
			if(auto ume=cast(UNotExp)e) return unExp("uNotExp", doIt(ume.e));
			if(auto ume=cast(UBitNotExp)e) return unExp("uBitNotExp", doIt(ume.e));
			if(auto le=cast(LambdaExp)e) {writeln("lambda");} //return qstate.makeFunction(le.fd);
			if(auto ce=cast(CallExp)e){
				auto id=cast(Identifier)unwrap(ce.e);
				auto fe=cast(FieldExp)unwrap(ce.e);
				auto fun=doIt(ce.e), arg=doIt(ce.arg);
				return expObj("callExp", operation(fun, arg));
			}if(auto fe=cast(ForgetExp)e){
			auto prop = jsonProp("var", dumpExp(fe.var));
			if(fe.val) prop~=jsonProp("val", dumpExp(fe.val));
			return expObj("forgetExp", prop);
			}if(auto idx=cast(IndexExp)e){
				auto a = doIt2(idx.e), i = doIt(idx.a);
				return expObj("indexExp", jsonProp("var", a)~jsonProp("index", i));
			}if(auto sl=cast(SliceExp)e){
				auto exp=doIt(sl.e), l=doIt(sl.l), r=doIt(sl.r);
				return expObj("sliceExp", jsonProp("var", exp)~jsonProp("left", l)~jsonProp("right", r));
			}if(auto le=cast(LiteralExp)e){
                return expObj("litExp", jsonProp("value", le.toString));
			}if(auto ite=cast(IteExp)e){
				auto cond=dumpExp(ite.cond);
				auto then=dumpStm(ite.then);
				auto othw=dumpStm(ite.othw);
				return expObj("iteExp", jsonProp("cond", cond)~jsonProp("then", then)~jsonProp("othw", othw));
			}else if(auto tpl=cast(TupleExp)e){
				auto values="["~tpl.e.map!(e=>doIt(e)).fold!((a,b)=>a~",\n"~b)~"]";
				return jsonProp("values", values);
			}else if(auto arr=cast(ArrayExp)e){
				auto et=arr.type.elementType;
				assert(!!et);
				auto values="["~arr.e.map!((e){
					auto value=doIt(e);
					if(e.type!=et) value~=jsonProp("type", "\""~et.toString~"\"")~jsonProp("consume", !arr.constLookup);
					return value;
				}).fold!((a,b)=>a~",\n"~b)~"]";
				return jsonProp("values", values);
			}else if(auto ae=cast(AssertExp)e){
				return expObj("assertExp", jsonProp("cond", dumpExp(ae.e)));
			}else if(auto tae=cast(TypeAnnotationExp)e){
				if(tae.e.type==tae.type) return doIt(tae.e);
				bool consume=!tae.constLookup;
				auto expr = doIt(tae.e);
                auto props = jsonProp("expr", expr)~jsonProp("type", "\""~tae.type.toString~"\"")~jsonProp("consume", consume);
                // if(tae.constLookup) r=r.consumeOnRead();
                return expObj("typeChangeExp", props);
			}else if(cast(Type)e){
				writeln("type");
			}else{
				enum common=q{
					auto e1=doIt(b.e1),e2=doIt(b.e2);
				};
				if(auto b=cast(AndExp)e){
					mixin(common);
					return binExp("andExp", e1, e2);
				}else if(auto b=cast(OrExp)e){
					mixin(common);
					return binExp("orExp", e1, e2);
				}else if(auto b=cast(LtExp)e){
					mixin(common);
					return binExp("ltExp", e1, e2);
				}else if(auto b=cast(LeExp)e){
					mixin(common);
					return binExp("leExp", e1, e2);
				}else if(auto b=cast(GtExp)e){
					mixin(common);
					return binExp("gtExp", e1, e2);
				}else if(auto b=cast(GeExp)e){
					mixin(common);
					return binExp("geExp", e1, e2);
				}else if(auto b=cast(EqExp)e){
					mixin(common);
					return binExp("eqExp", e1, e2);
				}else if(auto b=cast(NeqExp)e){
					mixin(common);
					return binExp("neqExp", e1, e2);
				}
			}
			enforce(0,text("ExpTODO: ",e," ",e.type));
			assert(0);
		}
		return doIt(e);
	}
}