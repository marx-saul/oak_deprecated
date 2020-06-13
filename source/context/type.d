module context.type;

import context.defs, parser.defs, parser.lexer;
/+
Type node_to_type(Node node) {
    if (node is null) return null;
    with (TokenType) switch (node.token.type) {
        case int_: case real_: case string_: case bool_:
        return new Type(node.token);

        case right_arrow:
        return new Type(node.token, node_to_type(node.child[0]), node_to_type(node.child[1]));

        case
    }
}
+/
