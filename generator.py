import random
import copy

def generate_variable() -> str:
    return chr(ord('a') + random.randint(0, 25)) + str(random.randint(0, 999))

def generate_integer() -> str:
    return str(random.randint(-100, 100))

def generate_boolean() -> str:
    return "#t" if random.random() < 0.5 else "#f"

def generate_datum(bound_vars: set) -> str:
    rand = random.random()
    if rand < 0.3 and len(bound_vars):
        for var in bound_vars:
            return var
    elif rand < 0.6:
        return generate_integer()
    else:
        return generate_boolean()

def generate_expression(depth: int, max_depth: int, bound_vars: set) -> str:
    if depth > max_depth:
        return generate_datum(bound_vars)
    
    op = random.choice(["if", "begin", "let", "letrec"])
    
    if op == "if":
        cond = generate_expression(depth + 1, max_depth, bound_vars)
        then_expr = generate_expression(depth + 1, max_depth, bound_vars)
        else_expr = generate_expression(depth + 1, max_depth, bound_vars)
        return f"({op} {cond} {then_expr} {else_expr})"
    
    elif op == "begin":
        exprs = [generate_expression(depth + 1, max_depth, bound_vars) for _ in range(random.randint(2, 4))]
        return f"({op} {' '.join(exprs)})"
    
    # elif op == "lambda":
    #     params = []
    #     bound_vars_new = copy.deepcopy(bound_vars)
    #     while len(params) < random.randint(2, 4):
    #         var = generate_variable()
    #         if var not in bound_vars:
    #             params.append(var)
    #             bound_vars_new.add(var)
    #     body = generate_expression(depth + 1, max_depth, bound_vars_new)
    #     return f"({op} ({','.join(params)}) {body})"
    
    elif op == "let":
        binds = []
        bound_vars_new = copy.deepcopy(bound_vars)
        while len(binds) < random.randint(2, 4):
            var = generate_variable()
            if var not in bound_vars:
                bound_vars_new.add(var)
                binds.append(f"[{var} (quote {generate_datum(bound_vars)})]")
        
        body = generate_expression(depth + 1, max_depth, bound_vars_new)
        return f"({op} ({' '.join(binds)}) {body})"
    
    elif op == "letrec":
        binds = []
        bound_vars_new = copy.deepcopy(bound_vars)
        while len(binds) < random.randint(2, 4):
            var = generate_variable()
            if var not in bound_vars:
                bound_vars_new.add(var)
                binds.append(f"[{var} (quote {generate_datum(bound_vars)})]")
        
        body = generate_expression(depth + 1, max_depth, bound_vars_new)
        return f"({op} ({' '.join(binds)}) {body})"
    
    else:
        raise ValueError("Invalid operation")

def generate_code(max_depth=10):
    bound_vars = set()
    return generate_expression(0, max_depth, bound_vars)

# Generate a 2K character Scheme code snippet
generated_code = generate_code(10)
print(generated_code)
print("(exit)")