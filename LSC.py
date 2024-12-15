import ply.lex as lex
import ply.yacc as yacc

# ---------------------------------
# Analisador Léxico
# ---------------------------------

tokens = (
    'IDENTIFIER', 'ASSIGN', 'NUMBER', 'IF', 'ELSE', 'GT', 'LT', 'GE', 'LE', 'EQ', 'NE'
)

reserved = {
    'if': 'IF',
    'else': 'ELSE'
}

literals = ['=', '+', '-', '*', '/', '(', ')', '{', '}', ',']

t_ASSIGN = r'='
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_NE = r'!='
t_ignore = ' \t'

def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return token.lexpos - line_start + 1

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')
    return t

def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

def t_error(t):
    print(f"Caractere inválido: {t.value[0]} (Linha {t.lineno}, Coluna {find_column(t.lexer.lexdata, t)})")
    t.lexer.skip(1)

lexer = lex.lex()

# ---------------------------------
# Analisador Sintático
# ---------------------------------

symbol_table = {}
c_code = []

precedence = (
    ('left', '+', '-'),
    ('left', '*', '/'),
)

def p_program_statements(p):
    """program : statement
               | program statement"""
    if len(p) == 2:  # Um único statement
        p[0] = p[1] if isinstance(p[1], list) else [p[1]]
    else:  # Múltiplos statements
        p[0] = p[1] + (p[2] if isinstance(p[2], list) else [p[2]])

def p_expression_relop(p):
    """expression : expression GT expression
                  | expression LT expression
                  | expression GE expression
                  | expression LE expression
                  | expression EQ expression
                  | expression NE expression"""
    if p[2] == '>':
        p[0] = {'value': p[1]['value'] > p[3]['value'], 'code': f"({p[1]['code']} > {p[3]['code']})"}
    elif p[2] == '<':
        p[0] = {'value': p[1]['value'] < p[3]['value'], 'code': f"({p[1]['code']} < {p[3]['code']})"}
    elif p[2] == '>=':
        p[0] = {'value': p[1]['value'] >= p[3]['value'], 'code': f"({p[1]['code']} >= {p[3]['code']})"}
    elif p[2] == '<=':
        p[0] = {'value': p[1]['value'] <= p[3]['value'], 'code': f"({p[1]['code']} <= {p[3]['code']})"}
    elif p[2] == '==':
        p[0] = {'value': p[1]['value'] == p[3]['value'], 'code': f"({p[1]['code']} == {p[3]['code']})"}
    elif p[2] == '!=':
        p[0] = {'value': p[1]['value'] != p[3]['value'], 'code': f"({p[1]['code']} != {p[3]['code']})"}


def p_statement_assign(p):
    """statement : IDENTIFIER ASSIGN expression"""
    symbol_table[p[1]] = p[3]['value']
    c_code_line = f"int {p[1]} = {p[3]['code']};"
    c_code.append(c_code_line)
    p[0] = [c_code_line]  # Retorna como lista para compor o bloco

def p_expression_binop(p):
    """expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression"""
    if p[2] == '+':
        p[0] = {'value': p[1]['value'] + p[3]['value'], 'code': f"({p[1]['code']} + {p[3]['code']})"}
    elif p[2] == '-':
        p[0] = {'value': p[1]['value'] - p[3]['value'], 'code': f"({p[1]['code']} - {p[3]['code']})"}
    elif p[2] == '*':
        p[0] = {'value': p[1]['value'] * p[3]['value'], 'code': f"({p[1]['code']} * {p[3]['code']})"}
    elif p[2] == '/':
        p[0] = {'value': p[1]['value'] / p[3]['value'], 'code': f"({p[1]['code']} / {p[3]['code']})"}

def p_if_else_statement(p):
    """statement : IF '(' expression ')' '{' program '}'
                 | IF '(' expression ')' '{' program '}' ELSE '{' program '}'"""
    if len(p) == 8:  # Apenas `if`
        c_code.append(f"if ({p[3]['code']}) {{")
        c_code.extend(p[6])  # Adiciona o código do bloco do `if`
        c_code.append("}")
    elif len(p) == 12:  # `if-else`
        c_code.append(f"if ({p[3]['code']}) {{")
        c_code.extend(p[6])  # Adiciona o código do bloco do `if`
        c_code.append("} else {")
        c_code.extend(p[10])  # Adiciona o código do bloco do `else`
        c_code.append("}")
       

def p_expression_number(p):
    """expression : NUMBER"""
    p[0] = {'value': p[1], 'code': str(p[1])}

def p_expression_identifier(p):
    """expression : IDENTIFIER"""
    var_name = p[1]
    if var_name in symbol_table:
        p[0] = {'value': symbol_table[var_name], 'code': var_name}
    else:
        print(f"Erro: variável '{var_name}' não definida.")
        p[0] = {'value': 0, 'code': '0'}

def p_error(p):
    if p:
        print(f"Erro de sintaxe no token '{p.value}' (Linha {p.lineno})")
    else:
        print("Erro de sintaxe: fim inesperado do arquivo.")

parser = yacc.yacc()

# ---------------------------------
# Código para Teste
# ---------------------------------

code = """
x = 10
if (x > 5) {
    y = x + 5
} else {
    y = x - 5
}
"""

print("Tokens gerados:")
lexer.input(code)
for token in lexer:
    print(token, f"(Linha {token.lineno}, Coluna {find_column(code, token)})")

print("\nAnalisador Sintático:")
parser.parse(code)

print("\nTabela de símbolos:")
for var, value in symbol_table.items():
    print(f"{var} = {value}")

# ---------------------------------
# Geração de Código em C
# ---------------------------------

c_code_output = "#include <stdio.h>\n\nint main() {\n"
c_code_output += "\n".join(f"    {line}" for line in c_code)
c_code_output += "\n    return 0;\n}"

print("\nCódigo Gerado em C:")
print(c_code_output)
