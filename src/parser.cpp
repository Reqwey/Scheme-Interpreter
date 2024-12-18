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
using std::string;
using std::vector;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Expr Syntax::parse(Assoc &env) {
  if (get() == nullptr)
    throw RuntimeError("unexpected EOF");
  return get()->parse(env);
}

Expr Number::parse(Assoc &env) { return Expr(new Fixnum(n)); }

Expr Identifier::parse(Assoc &env) {
  Value res = find(s, env);
  if (res.get())
    return Expr(new Var(s));

  switch (primitives[s]) {
  case E_VOID:
  case E_EXIT: {
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));

    st->stxs.push_back(new List());

    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  case E_MUL:
  case E_MINUS:
  case E_PLUS:
  case E_LT:
  case E_LE:
  case E_EQ:
  case E_GE:
  case E_GT:
  case E_EQQ:
  case E_CONS: {
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x")));
    args->stxs.push_back(Syntax(new Identifier("y")));
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));
    stx->stxs.push_back(Syntax(new Identifier("y")));
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args);
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  case E_BOOLQ:
  case E_INTQ:
  case E_NULLQ:
  case E_PAIRQ:
  case E_PROCQ:
  case E_SYMBOLQ:
  case E_NOT:
  case E_CAR:
  case E_CDR: {
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x")));
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args);
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  default:
    break;
  }

  return Expr(new Var(s));
}

Expr TrueSyntax::parse(Assoc &env) { return Expr(new True()); }

Expr FalseSyntax::parse(Assoc &env) { return Expr(new False()); }

#define checkArgc(num, arr, line)                                              \
  if (arr.size() - 1 != num) {                                                 \
    throw RuntimeError("Line " + std::to_string(line) + " expect " +           \
                       std::to_string(num) + " argument(s), found " +          \
                       std::to_string(arr.size() - 1));                        \
  }

Expr List::parse(Assoc &env) {
  if (stxs.empty()) {
    return Expr(new MakeVoid());
  }
  auto iden = dynamic_cast<Identifier *>(stxs[0].get());
  if (iden) {
    string s = iden->s;

    auto at_pri = primitives.find(s);
    auto at_res = reserved_words.find(s);

    Value res = find(s, env);
    if (res.get()) {
      goto apply;
    }
    // auto expression = dynamic_cast<Expression *>(res.get());
    // if (expression) {
    //   auto lam = dynamic_cast<Lambda *>(expression->e.get());
    //   if (lam)
    //     goto apply;
    // }

    if (at_pri == primitives.end() && !stxs.size()) {
      vector<Expr> rands;
      return Expr(new Apply(Expr(new Var(s)), rands));
    } else if (at_pri != primitives.end()) {
      switch (primitives[s]) {
      case E_MUL:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Mult(stxs[1].parse(env), stxs[2].parse(env)));

      case E_MINUS:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Minus(stxs[1].parse(env), stxs[2].parse(env)));

      case E_PLUS:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Plus(stxs[1].parse(env), stxs[2].parse(env)));

      case E_LT:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Less(stxs[1].parse(env), stxs[2].parse(env)));

      case E_LE:
        checkArgc(2, stxs, __LINE__);
        return Expr(new LessEq(stxs[1].parse(env), stxs[2].parse(env)));

      case E_EQ:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Equal(stxs[1].parse(env), stxs[2].parse(env)));

      case E_GE:
        checkArgc(2, stxs, __LINE__);
        return Expr(new GreaterEq(stxs[1].parse(env), stxs[2].parse(env)));

      case E_GT:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Greater(stxs[1].parse(env), stxs[2].parse(env)));

      case E_VOID:
        checkArgc(0, stxs, __LINE__);
        return Expr(new MakeVoid());

      case E_EQQ:
        checkArgc(2, stxs, __LINE__);
        return Expr(new IsEq(stxs[1].parse(env), stxs[2].parse(env)));

      case E_BOOLQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsBoolean(stxs[1].parse(env)));

      case E_INTQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsFixnum(stxs[1].parse(env)));

      case E_NULLQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsNull(stxs[1].parse(env)));

      case E_PAIRQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsPair(stxs[1].parse(env)));

      case E_PROCQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsProcedure(stxs[1].parse(env)));

      case E_SYMBOLQ:
        checkArgc(1, stxs, __LINE__);
        return Expr(new IsSymbol(stxs[1].parse(env)));

      case E_CONS:
        checkArgc(2, stxs, __LINE__);
        return Expr(new Cons(stxs[1].parse(env), stxs[2].parse(env)));

      case E_NOT:
        checkArgc(1, stxs, __LINE__);
        return Expr(new Not(stxs[1].parse(env)));

      case E_CAR:
        checkArgc(1, stxs, __LINE__);
        return Expr(new Car(stxs[1].parse(env)));

      case E_CDR:
        checkArgc(1, stxs, __LINE__);
        return Expr(new Cdr(stxs[1].parse(env)));

      case E_EXIT:
        checkArgc(0, stxs, __LINE__);
        return Expr(new Exit());

      default:
        break;
      }
    }

    if (at_res != reserved_words.end()) {
      switch (reserved_words[s]) {
      case E_LAMBDA: {
        checkArgc(2, stxs, __LINE__);

        auto args = (dynamic_cast<List *>(stxs[1].get()))->stxs;
        vector<string> transformedArgs;

        Assoc env1 = env;

        for (auto &syn : args) {
          string s = dynamic_cast<Identifier *>(syn.get())->s;
          transformedArgs.push_back(s);
          if (!find(s, env).get())
            env1 = extend(s, VoidV(), env1);
        }

        return Expr(new Lambda(transformedArgs, stxs[2].parse(env1)));
      }

      case E_LET: {
        checkArgc(2, stxs, __LINE__);

        auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
        vector<std::pair<string, Expr>> transformedHeader;

        Assoc env1 = env;

        for (auto &syn : header) {
          auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

          checkArgc(1, syn_v, __LINE__);

          string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
          Expr parsed = syn_v[1].parse(env);

          transformedHeader.push_back(std::make_pair(bind, parsed));
          env1 = extend(bind, ExpressionV(parsed), env1);
        }

        return Expr(new Let(transformedHeader, stxs[2].parse(env1)));
      }

      case E_LETREC: {
        checkArgc(2, stxs, __LINE__);

        auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
        vector<std::pair<string, Expr>> transformedHeader;

        Assoc env1 = env;

        for (auto &syn : header) {
          auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

          checkArgc(1, syn_v, __LINE__);

          string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
          Expr parsed = syn_v[1].parse(env);

          transformedHeader.push_back(std::make_pair(bind, parsed));
          env1 = extend(bind, ExpressionV(parsed), env1);
        }

        return Expr(new Letrec(transformedHeader, stxs[2].parse(env1)));
      }

      case E_IF:
        checkArgc(3, stxs, __LINE__);

        return Expr(
            new If(stxs[1].parse(env), stxs[2].parse(env), stxs[3].parse(env)));

      case E_BEGIN: {
        vector<Expr> es;
        for (size_t i = 1; i < stxs.size(); ++i)
          es.push_back(stxs[i].parse(env));
        return Expr(new Begin(es));
      }

      case E_QUOTE: {
        checkArgc(1, stxs, __LINE__);

        return Expr(new Quote(stxs[1]));
      }

      default:
        break;
      }
    }
  apply:
    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i)
      rands.push_back(stxs[i].parse(env));
    return Expr(new Apply(new Var(s), rands));
  }

  auto list = dynamic_cast<List *>(stxs[0].get());
  if (list) {
    // Expr parsed = list->parse(env);
    // auto lambda = dynamic_cast<Lambda *>(parsed.get());
    // if (lambda) {
    //   checkArgc(lambda->x.size(), this->stxs, __LINE__);

    //   Assoc env1 = env;

    //   vector<Expr> rands;
    //   for (size_t i = 1; i < stxs.size(); ++i) {
    //     Expr e = stxs[i].parse(env);
    //     rands.push_back(e);
    //     env1 = extend(lambda->x[i - 1], ExpressionV(e), env1);
    //   }

    //   return Expr(new Apply(list->parse(env1), rands));
    // } else {
    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i) {
      Expr e = stxs[i].parse(env);
      rands.push_back(e);
    }
    return Expr(new Apply(stxs[0].parse(env), rands));
    // }
  }

  throw RuntimeError("Unknown operation");
}

#endif