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
  Assoc env1 = Assoc(env);
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
      throw std::runtime_error(
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

  throw std::runtime_error("Bad function call");
} // for function calling

Value Letrec::eval(Assoc &env) {
  Assoc env1 = Assoc(env);

  for (auto &i : this->bind) {
    env1 = extend(i.first, NullV(), env1);
  }

  Assoc env2 = Assoc(env1);

  for (auto &i : this->bind) {
    modify(i.first, i.second.get()->eval(env1), env2);
  }

  return body.get()->eval(env2);
} // letrec expression

Value Var::eval(Assoc &e) {
  Value res = find(x, e);
  if (res.get())
    return res;
  else
    throw std::runtime_error("Unbound variable: " + x);

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
    es.erase(es.begin());
    return eval(e);
  }
} // begin expression

Value quoteFromSyn(Syntax s) {
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
      auto res = PairV(quoteFromSyn(list->stxs[sz - 1]), NullV());
      for (int i = sz - 2; i >= 0; --i)
        res = PairV(quoteFromSyn(list->stxs[i]), res);
      return res;
    }
  }

  return NullV();
}

Value Quote::eval(Assoc &e) { return quoteFromSyn(s); } // quote expression

Value MakeVoid::eval(Assoc &e) { return VoidV(); } // (void)

Value Exit::eval(Assoc &e) { return TerminateV(); } // (exit)

Value Binary::eval(Assoc &e) {
  return evalRator(rand1.get()->eval(e), rand2.get()->eval(e));
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
  return evalRator(rand.get()->eval(e));
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return IntegerV(val1 * val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return IntegerV(val1 + val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return IntegerV(val1 - val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return BooleanV(val1 < val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return BooleanV(val1 < val2 || val1 == val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return BooleanV(val1 == val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return BooleanV(val1 > val2 || val1 == val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
  try {
    int val1 = dynamic_cast<Integer *>(rand1.get())->n;
    int val2 = dynamic_cast<Integer *>(rand2.get())->n;
    return BooleanV(val1 > val2);
  } catch (std::bad_cast &) {
    throw std::runtime_error("Type error");
  }
} // >

bool isEqual(const Value &rand1, const Value &rand2) {
  if (rand1.get() == rand2.get())
    return true;

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
    return NullV();
} // car

Value Cdr::evalRator(const Value &rand) {
  auto pair = dynamic_cast<Pair *>(rand.get());
  if (pair)
    return Value(pair->cdr);
  else
    return NullV();
} // cdr
