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
  Assoc e = dynamic_cast<AssocList *>(env.get());
  for (auto &i : bind) {
    extend(i.first, i.second.get()->eval(env), e);
  }
  return body.get()->eval(e);
} // let expression

Value Lambda::eval(Assoc &env) {
  return ClosureV(x, e, env);
} // lambda expression

Value Apply::eval(Assoc &e) {
  auto lambda = dynamic_cast<Lambda *>(find(name, e).get());
  if (!lambda) throw std::runtime_error("Cannot find the function");

  Assoc e1 = dynamic_cast<AssocList *>(e.get());

  for (size_t i = 0; i < lambda->x.size(); ++i) {
    extend(lambda->x[i], this->rand[i].get()->eval(e), e1);
  }

  return lambda->e.get()->eval(e1);
} // for function calling

Value Letrec::eval(Assoc &env) {
  Assoc env1 = dynamic_cast<AssocList *>(env.get());

  for (auto &i : this->bind) {
    extend(i.first, NullV(), env1);
  }

  Assoc env2 = dynamic_cast<AssocList *>(env1.get());

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

  auto num = dynamic_cast<Fixnum *>(s.get());
  if (num)
    return IntegerV(num->n);

  auto iden = dynamic_cast<Identifier *>(s.get());
  if (iden) {
    return SymbolV(iden->s);
  }

  auto list = dynamic_cast<List *>(s.get());
  if (list) {
    switch (list->stxs.size()) {
    case 0:
      return NullV();
    case 1:
      return quoteFromSyn(list->stxs[0]);
    default:
      auto res =
          PairV(quoteFromSyn(list->stxs[0]), quoteFromSyn(list->stxs[1]));
      for (size_t i = 2; i < list->stxs.size(); ++i)
        res = PairV(res, quoteFromSyn(list->stxs[i]));
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

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
  auto sym1 = dynamic_cast<Symbol *>(rand1.get());
  auto sym2 = dynamic_cast<Symbol *>(rand2.get());
  if (sym1 && sym2) {
    return BooleanV(sym1->s == sym2->s);
  }

  auto num1 = dynamic_cast<Integer *>(rand1.get());
  auto num2 = dynamic_cast<Integer *>(rand2.get());
  if (num1 && num2) {
    return BooleanV(num1->n == num2->n);
  }

  // auto str1 = dynamic_cast<String *>(rand1.get());
  // auto str2 = dynamic_cast<String *>(rand2.get());
  // if (str1 && str2) {
  //   return BooleanV(str1->s == str2->s);
  // }

  return BooleanV(false);
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
  auto ptr = dynamic_cast<Fixnum *>(rand.get());
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
