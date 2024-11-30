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
  Value res = find(s, env);
  if (res.get())
    return Expr(new Var(s));

  switch (primitives[s]) {
  case E_VOID:
    return Expr(new MakeVoid());

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
    auto iden = dynamic_cast<Identifier *>(stxs[0].get());
    if (iden) {
      string s = iden->s;
      stxs.erase(stxs.begin());

      auto at_pri = primitives.find(s);
      auto at_res = reserved_words.find(s);

      Value res = find(s, env);
      auto expression = dynamic_cast<Expression *>(res.get());
      if (expression) {
        auto lam = dynamic_cast<Lambda *>(expression->e.get());
        if (lam)
          goto apply;
      }

      if (at_pri == primitives.end() && !stxs.size()) {
        return Expr(new Var(s));
      } else if (at_pri != primitives.end()) {
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
          checkArgc(1, stxs);
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
      }

      if (at_res != reserved_words.end()) {
        switch (reserved_words[s]) {
        case E_LAMBDA: {
          checkArgc(2, stxs);

          auto args = (dynamic_cast<List *>(stxs[0].get()))->stxs;
          vector<string> transformedArgs;

          Assoc env1 = Assoc(env);

          for (auto &syn : args) {
            string s = dynamic_cast<Identifier *>(syn.get())->s;
            transformedArgs.push_back(s);
            env1 = extend(s, VoidV(), env1);
          }

          return Expr(new Lambda(transformedArgs, stxs[1].parse(env)));
        }

        case E_LET: {
          checkArgc(2, stxs);

          auto header = (dynamic_cast<List *>(stxs[0].get()))->stxs;
          vector<std::pair<string, Expr>> transformedHeader;

          Assoc env1 = Assoc(env);

          for (auto &syn : header) {
            auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

            checkArgc(2, syn_v);

            string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
            Expr parsed = syn_v[1].parse(env);

            transformedHeader.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          return Expr(new Let(transformedHeader, stxs[1].parse(env1)));
        }

        case E_LETREC: {
          checkArgc(2, stxs);

          auto header = (dynamic_cast<List *>(stxs[0].get()))->stxs;
          vector<std::pair<string, Expr>> transformedHeader;

          Assoc env1 = Assoc(env);

          for (auto &syn : header) {
            auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

            checkArgc(2, syn_v);

            string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
            Expr parsed = syn_v[1].parse(env);

            transformedHeader.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          return Expr(new Letrec(transformedHeader, stxs[1].parse(env1)));
        }

        case E_IF:
          checkArgc(3, stxs);

          return Expr(new If(stxs[0].parse(env), stxs[1].parse(env),
                             stxs[2].parse(env)));

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
          break;
        }
      }
    apply:
      vector<Expr> rands;
      for (auto &i : stxs)
        rands.push_back(i.parse(env));
      return Expr(new Apply(new Var(s), rands));
    }

    auto list = dynamic_cast<List *>(stxs[0].get());
    if (list) {
      vector<Expr> rands;
      for (size_t i = 1; i < stxs.size(); ++i) {
        rands.push_back(stxs[i].parse(env));
      }

      return Expr(new Apply(list->parse(env), rands));
    }

    throw runtime_error("Unknown operation");
  } catch (std::bad_cast &) {
    throw runtime_error("The object is not applicable");
  } catch (std::runtime_error &msg) {
    throw msg;
  }
}

#endif