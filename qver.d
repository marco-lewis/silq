// Python Scripting
// https://dlang.org/phobos/std_process.html
// https://pyd.readthedocs.io/en/latest/embed.html

// Verification
// Build up expressions
// Pauli gates plus Hadamard
// Then measurement
// Handle some basic classical functionality (B, !N, !Q etc)
// Then vectors, arrays etc (int[n], uint[n])
// If-then-else statements
// Complex classical func

// Build up statements
// Def, Assign
// Call, Return, FunctionDef
// Observe
// ITE
// Loops

import std.conv: text, to;
import std.string: split;
import std.algorithm;
import std.array: array;
import std.range: iota, zip, repeat, walkLength;
import std.string: startsWith,join;
import std.typecons: q=tuple,Q=Tuple;
import std.exception: enforce;

import options;
import astopt;
import util.hashtable,util;
import ast.expression,ast.declaration,ast.type;
import ast.lexer,ast.semantic_,ast.reverse,ast.scope_,ast.error;
import util;

import std.random, sample;
import std.complex, std.math;

class QVer{
	this(string sourceFile){
		this.sourceFile=sourceFile;
	}
	bool verify(FunctionDef[string] defs){
		// Create an interpreter
		// Job: read a function and convert it to a proof obligation (with any summaries that have been made)
		// Either call scripts or create SMTLIB in D
		auto interpreter=VerInterpreter!VariableTracker(false);
		writeln("Interpreter made");
		
		// Collect all functions needed from other files
		// Order functions so that lowest level function is verified first
		// interpreter.fetchFunctions(defs);
		writeln("Functions fetched");
		writeln("Functions sorted");

		// For each function
		foreach(func_name, def;defs){
			writeln(func_name);
			// Generate proof obligations through interpreter
			CompoundExp statements = def.body_;
			foreach (s; statements.s){
				interpreter.verifStm(s);
			}
			// Look up stored verified functions or use function summary
			// Send to a solver to verify
			// Store verified obligations
		}
		writeln("Proof obligations generated");
		// Read result and give information

		return false;
	}
private:
	string sourceFile;
}

// Keep track of variable names
// TODO: Make efficient
struct VariableTracker{
	alias Record=HashMap!(string,string,(a,b)=>a==b,(a)=>typeid(a).getHash(&a));
	alias Timer=HashMap!(string,int,(a,b)=>a==b,(a)=>typeid(a).getHash(&a));
	string[string] quantumVars;
	int[string] timer;

	void initializeVar(string var){
		quantumVars[var] = var;
		timer[var] = 0;
	}

	void updateTimer(string var){
		const auto s = quantumVars[var];
		timer[s] += 1;
	}

	void addVar(string newVar, string oldVar){
		const auto s = quantumVars[oldVar];
		quantumVars[newVar] = s;
	}

	bool isVar(string var){
		return (var in quantumVars) !is null;
	}

	string getVerifToken(string var){
		const auto s = quantumVars[var];
		return "q"~cast(string)s~"_t"~text(timer[s]);
	}
}

struct VerInterpreter(VariableTracker){
	bool verified;
	VariableTracker tracker;

	this(bool verified){
		this.verified = verified;
	}
	// FunctionDef functionDef;
	// CompoundExp statements;
	// QState qstate;
	// bool hasFrame=false;
	// this(FunctionDef functionDef,CompoundExp statements,QState qstate,bool hasFrame){
	// 	this.functionDef=functionDef;
	// 	this.statements=statements;
	// 	this.qstate=qstate;
	// 	this.hasFrame=hasFrame;
	// }

	// TODO: Implement properly
	void fetchFunctions(FunctionDef[string] defs){
		// foreach(func_name, def;defs){
		// 	CompoundExp statements = def.body_;
		// 	foreach(s; statements.s){
		// 		this.verifStm(s);
		// 	}
		// }
	}

	void verifStm(Expression e){
		// TODO: Error - try and catch
		return verifStm2(e);
	}
	void verifStm2(Expression e){
	// 	if(opt.trace && !isInPrelude(functionDef)){
	// 		writeln(qstate);
	// 		writeln();
	// 		writeln("STATEMENT");
	// 		writeln(e);
	// 		writeln();
	// 	}
		if(auto nde=cast(DefExp)e){
			auto de=cast(DefineExp)nde.initializer;
			verifStm2(de);
		}else if(auto ae=cast(AssignExp)e){
			auto lhs=ae.e1,rhs=ae.e2;
			writeln(lhs,":", rhs);
		}else if(auto ae=cast(DefineExp)e){
			if(ae.isSwap){
				auto tpl=cast(TupleExp)unwrap(ae.e2);
				enforce(!!tpl);
				writeln(tpl.e[0], "<->", tpl.e[1]);
			}else{
				auto lhs=ae.e1.toString(),rhs=ae.e2;
				auto r=verifExp(rhs);
				writeln("r: ", r);
				writeln(lhs,"=", rhs);
				if (!tracker.isVar(lhs)){
					tracker.initializeVar(lhs);
				}
				// Create proof obligations
				writeln(tracker.getVerifToken(lhs), " ", r);
				// Update timer
				tracker.updateTimer(lhs);
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
	// 	}else if(auto call=cast(CallExp)e){
	// 	}else if(auto ce=cast(CompoundExp)e){
	// 	}else if(auto ite=cast(IteExp)e){
	// 	}else if(auto re=cast(RepeatExp)e){
	// 	}else if(auto fe=cast(ForExp)e){
	// 	}else if(auto we=cast(WhileExp)e){
	// 	}else if(auto re=cast(ReturnExp)e){
	// 	}else if(auto ae=cast(AssertExp)e){
	// 	}else if(auto oe=cast(ObserveExp)e){
	// 	}else if(auto fe=cast(ForgetExp)e){
	// 	}else if(auto ce=cast(CommaExp)e){
	// 	}else if(auto fd=cast(FunctionDef)e){
	// 	}else if(cast(Declaration)e){
	// 		// do nothing
		// }
		else{
			enforce(0,text("StmtTODO: ",e));
		}
	}


	string verifExp(Expression e){
	// 	if(!qstate.state.length) return QState.Value.init;
		string doIt()(Expression e){
			// TODO: errors - try, catch
			return doIt2(e);
		}
	// 	// TODO: get rid of code duplication
		string doIt2(Expression e){
	// 		if(e.type == typeTy) return QState.typeValue; // TODO: get rid of this
			if(auto id=cast(Identifier)e){
				if(id.substitute){
					if(auto vd=cast(VarDecl)id.meaning)
						auto r = doIt2(vd.initializer);
				}
				// This changes qstate which affects CallExp cases
				// auto r=lookupMeaning(qstate,id);
				// enforce(r.isValid,"unsupported");
				// return r;
				writeln("id ", id.meaning);
				return "";
				// return tracker.getVerifToken(false, id.name);
			}
			if(auto fe=cast(FieldExp)e){
				enforce(fe.type.isClassical||fe.constLookup);
				if(isBuiltIn(fe)){
					if(auto at=cast(ArrayTy)fe.e.type){
						assert(fe.f.name=="length");
						writeln("bin-Field");
						doIt(fe.e);
						return "";
						// enforce(r.tag==QState.Value.Tag.array_);
						// return qstate.makeInteger(ℤ(r.array_.length));
					}
				}
				// TODO: non-constant field lookup
				// return qstate.readField(doIt(fe.e),fe.f.name,true);
				writeln("Field");
				doIt(fe.e);
				return "";
			}
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
				string thisExp = "";
				writeln("call");
				if(fe){
					writeln("fe");
					id=fe.f;
					thisExp=doIt(fe.e);
				}
				if(id){
					writeln("id");
					if(!fe && isBuiltIn(id)){
						switch(id.name){
							static if(language==silq){
								case "quantumPrimitive":
									enforce(0,"quantum primitive cannot be used as first-class value");
									assert(0);
								// case "__show","__query":
									// return qstate.makeTuple(ast.type.unit,[]);
							}
							default:
								enforce(0,text("quantTODO: ",id.name));
								assert(0);
						}
					}
				}else if(auto ce2=cast(CallExp)unwrap(ce.e)){
					writeln("ce2");
					if(auto id2=cast(Identifier)unwrap(ce2.e)){
						if(isBuiltIn(id2)){
							switch(id2.name){
								static if(language==silq) case "quantumPrimitive":
									switch(getQuantumOp(ce2.arg)){
										// case "dup": enforce(0,"quantumPrimitive(\"dup\")[τ] cannot be used as first-class value"); assert(0);
										// case "array": enforce(0,"quantumPrimitive(\"array\")[τ] cannot be used as first-class value"); assert(0);
										// case "vector": enforce(0,"quantumPrimitive(\"vector\")[τ] cannot be used as first-class value"); assert(0);
										// case "reverse":  enforce(0,"quantumPrimitive(\"reverse\")[τ] cannot be used as first-class value"); assert(0);
										// case "M": enforce(0,"quantumPrimitive(\"M\")[τ] cannot be used as first-class value"); assert(0);
										case "H": writeln("H");doIt(ce.arg);return "";
										case "X": writeln("X");doIt(ce.arg);return "";
										case "Y": writeln("Y");doIt(ce.arg);return "";
										case "Z": writeln("Z");doIt(ce.arg);return "";
										// case "P": return qstate.phase(doIt(ce.arg));
										// case "rX": return qstate.rX(doIt(ce.arg));
										// case "rY": return qstate.rY(doIt(ce.arg));
										// case "rZ": return qstate.rZ(doIt(ce.arg));
										default: break;
									}
									break;
								default:
									break;
							}
						}
					}else if(auto ce3=cast(CallExp)unwrap(ce2.e)){
						writeln("ce3");
						if(auto id3=cast(Identifier)unwrap(ce3.e)){
							if(isBuiltIn(id3)){
								switch(id3.name){
									static if(language==silq) case "quantumPrimitive":
										switch(getQuantumOp(ce3.arg)){
								// 			case "dup": return doIt(ce.arg).dup(qstate);
								// 			case "array": return qstate.array_(ce.type,doIt(ce.arg));
								// 			case "vector": return qstate.vector(ce.type,doIt(ce.arg));
								// 			case "reverse": enforce(0); break;
								// 			case "M": return qstate.measure(doIt(ce.arg));
											default: break;
										}
										break;
									default:
										break;
								}
							}
						}
					}
				}
				// auto fun=doIt(ce.e), arg=doIt(ce.arg);
				// return qstate.call(fun,arg,ce.type,ce.loc);
				doIt(ce.e);
				doIt(ce.arg);
				return "";
			}
	// 		if(auto fe=cast(ForgetExp)e){
	// 		}
	// 		if(auto idx=cast(IndexExp)e){
	// 		}
	// 		if(auto sl=cast(SliceExp)e){
	// 		}
			if(auto le=cast(LiteralExp)e){
				// Need to handle different numbers here (rational, reals etc.)
				writeln("lit ", le);
				return le.toString();
			}
	// 		if(auto ite=cast(IteExp)e){
	// 		}else if(auto tpl=cast(TupleExp)e){
	// 		}else if(auto arr=cast(ArrayExp)e){
	// 		}else if(auto ae=cast(AssertExp)e){
			else if(auto tae=cast(TypeAnnotationExp)e){
				// if(tae.e.type==tae.type) return doIt(tae.e);
				// bool consume=!tae.constLookup;
				// auto r=convertTo(doIt(tae.e),tae.type,consume);
				// if(tae.constLookup) r=r.consumeOnRead();
				// return r;
				writeln("TAE ", tae);
				auto r=doIt(tae.e);
				return r;
			}
	// 		}else if(cast(Type)e)
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

	// void run(ref QState retState){
	// 	foreach(s;statements.s){
	// 		runStm(s,retState);
	// 		// writeln("cur: ",cur);
	// 	}
	// }
	// void runFun(ref QState retState){
	// 	run(retState);
	// 	retState+=qstate;
	// 	qstate=QState.empty();
	// }
}