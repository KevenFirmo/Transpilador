import ply.lex as lex
import ply.yacc as yacc

# ---------------------------------
# Analisador Léxico
# ---------------------------------

tokens = (
    'IDENTIFIER', 'ASSIGN', 'NUMBER'
)

literals = ['=', '+', '-', '*', '/']

t_ASSIGN = r'='
t_ignore = ' \t'

def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return token.lexpos - line_start + 1

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
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
    pass

def p_statement_assign(p):
    """statement : IDENTIFIER ASSIGN expression"""
    symbol_table[p[1]] = p[3]['value']  # Armazena o valor semântico
    c_code.append(f"int {p[1]} = {p[3]['code']};")  # Gera o código em C
    print(f"Variável '{p[1]}' atribuída com o valor: {p[3]['value']}")

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
y = x + 5
z = y * 2
w = z - x
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

