#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <cstring>
#include <map>
#include <vector>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Let::eval(Assoc &env) {
  Assoc env1 = env;
  for (auto &i : bind) {
    env1 = extend(i.first, i.second.get()->eval(env), env1);
  }
  return body.get()->eval(env1);
} // let expression

Value Lambda::eval(Assoc &env) {
  return ClosureV(x, e, env);
} // lambda expression

Value Apply::eval(Assoc &env) {
  Value rator = this->rator.get()->eval(env);

  auto closure = dynamic_cast<Closure *>(rator.get());
  if (closure) {
    if (closure->parameters.size() != this->rand.size()) {
      throw RuntimeError(
          "Expect " + std::to_string(closure->parameters.size()) +
          " argument(s), found " + std::to_string(this->rand.size()));
    }

    Assoc env1 = Assoc(closure->env);
    for (size_t i = 0; i < closure->parameters.size(); ++i) {
      env1 =
          extend(closure->parameters[i], this->rand[i].get()->eval(env), env1);
    }

    return closure->e.get()->eval(env1);
  }

  throw RuntimeError("Bad function call");
} // for function calling

Value Letrec::eval(Assoc &env) {
  Assoc env1 = env;
  for (auto &i : this->bind) {
    env1 = extend(i.first, NullV(), env1);
  }

  Assoc env2 = env;

  for (auto &i : this->bind) {
    Value val = i.second.get()->eval(env1);
    if (val.get()->v_type == V_NULL)
      throw RuntimeError("Unusable variable");
    env2 = extend(i.first, val, env2);
  }

  for (auto &i : this->bind) {
    Value val = find(i.first, env2);    
    modify(i.first, i.second.get()->eval(env2), env2);
  }

  return body.get()->eval(env2);
} // letrec expression

Value Var::eval(Assoc &e) {
  Value res = find(x, e);
  if (res.get())
    return res;
  else
    throw RuntimeError("Unbound variable: " + x);

} // evaluation of variable

Value Fixnum::eval(Assoc &e) { return IntegerV(n); } // evaluation of a fixnum

Value If::eval(Assoc &e) {
  Value res = cond.get()->eval(e);

  auto bool_res = dynamic_cast<Boolean *>(res.get());
  if (bool_res && bool_res->b == false)
    return alter.get()->eval(e);
  else
    return conseq.get()->eval(e);
} // if expression

Value True::eval(Assoc &e) { return BooleanV(true); } // evaluation of #t

Value False::eval(Assoc &e) { return BooleanV(false); } // evaluation of #f

Value Begin::eval(Assoc &e) {
  switch (es.size()) {
  case 0:
    return NullV();
  case 1:
    return es[0].get()->eval(e);
  default:
    es[0].get()->eval(e);
    es.erase(es.begin());
    return eval(e);
  }
} // begin expression

Value Quote::eval(Assoc &e) {
  auto bool_f = dynamic_cast<FalseSyntax *>(s.get());
  if (bool_f)
    return BooleanV(false);

  auto bool_t = dynamic_cast<TrueSyntax *>(s.get());
  if (bool_t)
    return BooleanV(true);

  auto num = dynamic_cast<Number *>(s.get());
  if (num)
    return IntegerV(num->n);

  auto iden = dynamic_cast<Identifier *>(s.get());
  if (iden) {
    return SymbolV(iden->s);
  }

  auto list = dynamic_cast<List *>(s.get());
  if (list) {
    if (list->stxs.size() == 0) {
      return NullV();
    } else {
      size_t sz = list->stxs.size();
      if (sz >= 3) {
        auto isDot = dynamic_cast<Identifier *>(list->stxs[sz - 2].get());
        if (isDot && isDot->s == ".") {
          Value res = Expr(new Quote(list->stxs[sz - 1])).get()->eval(e);
          for (int i = sz - 3; i >= 0; --i)
            res = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), res);
          return res;
        }
      }
      Value res = NullV();
      for (int i = sz - 1; i >= 0; --i)
        res = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), res);
      return res;
    }
  }

  return NullV();
} // quote expression

Value MakeVoid::eval(Assoc &e) { return VoidV(); } // (void)

Value Exit::eval(Assoc &e) { return TerminateV(); } // (exit)

Value Binary::eval(Assoc &e) {
  return evalRator(rand1.get()->eval(e), rand2.get()->eval(e));
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
  return evalRator(rand.get()->eval(e));
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return IntegerV(val1->n * val2->n);
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return IntegerV(val1->n + val2->n);
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return IntegerV(val1->n - val2->n);
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return BooleanV(val1->n < val2->n);
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return BooleanV(val1->n <= val2->n);
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return BooleanV(val1->n == val2->n);
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return BooleanV(val1->n >= val2->n);
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
  auto val1 = dynamic_cast<Integer *>(rand1.get());
  auto val2 = dynamic_cast<Integer *>(rand2.get());
  if (!val1 || !val2)
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
  return BooleanV(val1->n > val2->n);
} // >

bool isEqual(const Value &rand1, const Value &rand2) {
  if (rand1.get() == rand2.get())
    return true;

  auto bool1 = dynamic_cast<Boolean *>(rand1.get());
  auto bool2 = dynamic_cast<Boolean *>(rand2.get());
  if (bool1 && bool2) {
    return bool1->b == bool2->b;
  }

  auto sym1 = dynamic_cast<Symbol *>(rand1.get());
  auto sym2 = dynamic_cast<Symbol *>(rand2.get());
  if (sym1 && sym2) {
    return sym1->s == sym2->s;
  }

  auto num1 = dynamic_cast<Integer *>(rand1.get());
  auto num2 = dynamic_cast<Integer *>(rand2.get());
  if (num1 && num2) {
    return num1->n == num2->n;
  }

  auto nul1 = dynamic_cast<Null *>(rand1.get());
  auto nul2 = dynamic_cast<Null *>(rand2.get());
  if (nul1 && nul2) {
    return true;
  }

  auto void1 = dynamic_cast<Void *>(rand1.get());
  auto void2 = dynamic_cast<Void *>(rand2.get());
  if (void1 && void2) {
    return true;
  }

  // auto str1 = dynamic_cast<String *>(rand1.get());
  // auto str2 = dynamic_cast<String *>(rand2.get());
  // if (str1 && str2) {
  //   return str1->s == str2->s;
  // }

  auto var1 = dynamic_cast<Var *>(rand1.get());
  auto var2 = dynamic_cast<Var *>(rand2.get());
  if (var1 && var2) {
    return var1->x == var2->x;
  }

  return false;
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
  return BooleanV(isEqual(rand1, rand2));
} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
  return PairV(rand1, rand2);
} // cons

Value IsBoolean::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Boolean *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Integer *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // fixnum?

Value IsSymbol::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Symbol *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // symbol?

Value IsNull::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Null *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // null?

Value IsPair::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Pair *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // pair?

Value IsProcedure::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Closure *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // procedure?

Value Not::evalRator(const Value &rand) {
  auto _f = dynamic_cast<Boolean *>(rand.get());
  if (_f && _f->b == false)
    return BooleanV(true);
  else
    return BooleanV(false);
} // not

Value Car::evalRator(const Value &rand) {
  auto pair = dynamic_cast<Pair *>(rand.get());
  if (pair)
    return Value(pair->car);
  else
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
} // car

Value Cdr::evalRator(const Value &rand) {
  auto pair = dynamic_cast<Pair *>(rand.get());
  if (pair)
    return Value(pair->cdr);
  else
    throw RuntimeError("Type error on line " + std::to_string(__LINE__));
} // cdr
