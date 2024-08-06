#ifndef PARSER
#define PARSER

// parser of myscheme

#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <algorithm>
#include <cstring>
#include <iostream>
#include <map>
using std::pair;
using std::runtime_error;
using std::string;
using std::vector;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Expr Syntax::parse(Assoc &env) {
  if (get() == nullptr)
    throw runtime_error("unexpected EOF");
  return get()->parse(env);
}

Expr Number::parse(Assoc &env) { return Expr(new Fixnum(n)); }

Expr Identifier::parse(Assoc &env) {
  switch (primitives[s]) {
  case E_MUL:
    return Expr(new Mult(nullptr, nullptr));

  case E_MINUS:
    return Expr(new Minus(nullptr, nullptr));

  case E_PLUS:
    return Expr(new Plus(nullptr, nullptr));

  case E_LT:
    return Expr(new Less(nullptr, nullptr));

  case E_LE:
    return Expr(new LessEq(nullptr, nullptr));

  case E_EQ:
    return Expr(new Equal(nullptr, nullptr));

  case E_GE:
    return Expr(new GreaterEq(nullptr, nullptr));

  case E_GT:
    return Expr(new Greater(nullptr, nullptr));

  case E_VOID:
    return Expr(new MakeVoid());

  case E_EQQ:
    return Expr(new IsEq(nullptr, nullptr));

  case E_BOOLQ:
    return Expr(new IsBoolean(nullptr));

  case E_INTQ:
    return Expr(new IsFixnum(nullptr));

  case E_NULLQ:
    return Expr(new IsNull(nullptr));

  case E_PAIRQ:
    return Expr(new IsPair(nullptr));

  case E_PROCQ:
    return Expr(new IsProcedure(nullptr));

  case E_SYMBOLQ:
    return Expr(new IsSymbol(nullptr));

  case E_CONS:
    return Expr(new Cons(nullptr, nullptr));

  case E_NOT:
    return Expr(new Not(nullptr));

  case E_CAR:
    return Expr(new Car(nullptr));

  case E_CDR:
    return Expr(new Cdr(nullptr));

  case E_EXIT:
    return Expr(new Exit());

  default:
    break;
  }

  return Expr(new Var(s));
}

Expr TrueSyntax::parse(Assoc &env) { return Expr(new True()); }

Expr FalseSyntax::parse(Assoc &env) { return Expr(new False()); }

#define checkArgc(num, arr)                                                    \
  if (arr.size() != num) {                                                     \
    throw runtime_error("Expect " #num " argument(s), found " +                \
                        std::to_string(arr.size()));                           \
  }

Expr List::parse(Assoc &env) {
  if (stxs.empty()) {
    return Expr(new MakeVoid());
  }
  try {
    string s = (dynamic_cast<Identifier *>(stxs[0].get()))->s;
    stxs.erase(stxs.begin());

    switch (primitives[s]) {
    case E_MUL:
      checkArgc(2, stxs);
      return Expr(new Mult(stxs[0].parse(env), stxs[1].parse(env)));

    case E_MINUS:
      checkArgc(2, stxs);
      return Expr(new Minus(stxs[0].parse(env), stxs[1].parse(env)));

    case E_PLUS:
      checkArgc(2, stxs);
      return Expr(new Plus(stxs[0].parse(env), stxs[1].parse(env)));

    case E_LT:
      checkArgc(2, stxs);
      return Expr(new Less(stxs[0].parse(env), stxs[1].parse(env)));

    case E_LE:
      checkArgc(2, stxs);
      return Expr(new LessEq(stxs[0].parse(env), stxs[1].parse(env)));

    case E_EQ:
      checkArgc(2, stxs);
      return Expr(new Equal(stxs[0].parse(env), stxs[1].parse(env)));

    case E_GE:
      checkArgc(2, stxs);
      return Expr(new GreaterEq(stxs[0].parse(env), stxs[1].parse(env)));

    case E_GT:
      checkArgc(2, stxs);
      return Expr(new Greater(stxs[0].parse(env), stxs[1].parse(env)));

    case E_VOID:
      checkArgc(0, stxs);
      return Expr(new MakeVoid());

    case E_EQQ:
      checkArgc(2, stxs);
      return Expr(new IsEq(stxs[0].parse(env), stxs[1].parse(env)));

    case E_BOOLQ:
      checkArgc(2, stxs);
      return Expr(new IsBoolean(stxs[0].parse(env)));

    case E_INTQ:
      checkArgc(1, stxs);
      return Expr(new IsFixnum(stxs[0].parse(env)));

    case E_NULLQ:
      checkArgc(1, stxs);
      return Expr(new IsNull(stxs[0].parse(env)));

    case E_PAIRQ:
      checkArgc(1, stxs);
      return Expr(new IsPair(stxs[0].parse(env)));

    case E_PROCQ:
      checkArgc(1, stxs);
      return Expr(new IsProcedure(stxs[0].parse(env)));

    case E_SYMBOLQ:
      checkArgc(1, stxs);
      return Expr(new IsSymbol(stxs[0].parse(env)));

    case E_CONS:
      checkArgc(2, stxs);
      return Expr(new Cons(stxs[0].parse(env), stxs[1].parse(env)));

    case E_NOT:
      checkArgc(1, stxs);
      return Expr(new Not(stxs[0].parse(env)));

    case E_CAR:
      checkArgc(1, stxs);
      return Expr(new Car(stxs[0].parse(env)));

    case E_CDR:
      checkArgc(1, stxs);
      return Expr(new Cdr(stxs[0].parse(env)));

    case E_EXIT:
      checkArgc(0, stxs);
      return Expr(new Exit());

    default:
      break;
    }

    switch (reserved_words[s]) {
    case E_LET: {
      Assoc env1 = dynamic_cast<AssocList *>(env.get());
      checkArgc(2, stxs);

      auto header = (dynamic_cast<List *>(stxs[0].get()))->stxs;
      vector<std::pair<string, Expr>> transformedHeader;

      for (auto &syn : header) {
        auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

        checkArgc(2, syn_v);

        try {
          string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;

          if (primitives[bind])
            throw std::bad_cast();

          Expr syn = syn_v[1].parse(env);
          auto isLambda = dynamic_cast<Lambda *>(syn.get());

          if (isLambda) {
            extend(bind, ClosureV(isLambda->x, isLambda->e, env1), env1);
          }

          transformedHeader.push_back(std::make_pair(bind, syn));
        } catch (std::bad_cast &) {
          throw runtime_error("The object is not bindable");
        }
      }

      return Expr(new Let(transformedHeader, stxs[1].parse(env1)));
    }

    case E_LAMBDA: {
      checkArgc(2, stxs);

      auto args = (dynamic_cast<List *>(stxs[0].get()))->stxs;
      vector<string> transformedArgs;

      for (auto &syn : args) {
        transformedArgs.push_back(dynamic_cast<Identifier *>(syn.get())->s);
      }

      return Expr(new Lambda(transformedArgs, stxs[1].parse(env)));
    }

    case E_LETREC: {
      Assoc env1 = dynamic_cast<AssocList *>(env.get());
      checkArgc(2, stxs);

      auto header = (dynamic_cast<List *>(stxs[0].get()))->stxs;
      vector<std::pair<string, Expr>> transformedHeader;

      for (auto &syn : header) {
        auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

        checkArgc(2, syn_v);

        try {
          string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;

          if (primitives[bind])
            throw std::bad_cast();

          Expr syn = syn_v[1].parse(env);
          auto isLambda = dynamic_cast<Lambda *>(syn.get());

          if (isLambda) {
            extend(bind, ClosureV(isLambda->x, isLambda->e, env1), env1);
          }

          transformedHeader.push_back(std::make_pair(bind, syn));
        } catch (std::bad_cast &) {
          throw runtime_error("The object is not bindable");
        }
      }

      return Expr(new Letrec(transformedHeader, stxs[1].parse(env1)));
    }

    case E_IF:
      checkArgc(3, stxs);

      return Expr(
          new If(stxs[0].parse(env), stxs[1].parse(env), stxs[2].parse(env)));

    case E_BEGIN: {
      vector<Expr> es;
      for (auto &i : stxs)
        es.push_back(i.parse(env));
      return Expr(new Begin(es));
    }

    case E_QUOTE: {
      checkArgc(1, stxs);

      return Expr(new Quote(stxs[0]));
    }

    default:
      auto fn = dynamic_cast<Closure *>(find(s, env).get());

      if (fn) {
        checkArgc(fn->parameters.size(), stxs);

        vector<Expr> es;
        for (auto &i : stxs)
          es.push_back(i.parse(env));

        return Expr(new Apply(s, es));
      }

      throw runtime_error("Unknown operation");
    }
  } catch (std::bad_cast &) {
    throw runtime_error("The object is not applicable");
  }
}

#endif