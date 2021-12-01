const PREC = {
  COMMENT: -2,
  CURLY_BLOCK: 1,
  DO_BLOCK: -1,

  AND: -2,
  OR: -2,
  NOT: 5,
  DEFINED: 10,
  ALIAS: 11,
  ASSIGN: 15,
  RESCUE: 16,
  CONDITIONAL: 20,
  RANGE: 25,
  BOOLEAN_OR: 30,
  BOOLEAN_AND: 35,
  RELATIONAL: 40,
  COMPARISON: 45,
  BITWISE_OR: 50,
  BITWISE_AND: 55,
  CALL: 56,
  SHIFT: 60,
  ADDITIVE: 65,
  MULTIPLICATIVE: 70,
  UNARY_MINUS: 75,
  EXPONENTIAL: 80,
  COMPLEMENT: 85,
}

const IDENTIFIER_CHARS = /[^\x00-\x1F\s:;`"'@$#.,|^&<=>+\-*/\\%?!~()\[\]{}]*/
const LOWER_ALPHA_CHAR = /[^\x00-\x1F\sA-Z0-9:;`"'@$#.,|^&<=>+\-*/\\%?!~()\[\]{}]/
const ALPHA_CHAR = /[^\x00-\x1F\s0-9:;`"'@$#.,|^&<=>+\-*/\\%?!~()\[\]{}]/

module.exports = grammar({
  name: 'ruby',

  externals: $ => [
    $._line_break,

    // Delimited literals
    $.simple_symbol,
    $._string_start,
    $._symbol_start,
    $._subshell_start,
    $._regex_start,
    $._string_array_start,
    $._symbol_array_start,
    $._heredoc_body_start,
    $.string_content,
    $.heredoc_content,
    $._string_end,
    $.heredoc_end,
    $.heredoc_beginning,

    // Tokens that require lookahead
    '/',
    $._block_ampersand,
    $._splat_star,
    $._unary_minus,
    $._binary_minus,
    $._binary_star,
    $._singleton_class_left_angle_left_langle,
    $.hash_key_symbol,
    $._hash_splat_star_star,
    $._binary_star_star,
    $._element_reference_bracket,
  ],

  extras: $ => [$.comment, $.heredoc_body, /\s|\\\n/],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.unless, $.terminator],
    [$.when, $.terminator],
    [$.expression_, $.pair],
    [$.expression_, $._chained_command_call],
    [$.command_argument_list, $.argument_list],
    [$.primary, $.chained_string],
    [$.chained_string],
    [$._chained_command_call, $._variable],
    [$.begin, $.primary],
    [$.member, $.statement],
  ],

  supertypes: $ => [$._method_name, $._variable],

  rules: {
    source_file: $ =>
      seq(
        optional_with_placeholder('statement_list', $.statement_list),
        optional(seq('__END__', $._line_break, $.uninterpreted))
      ),

    uninterpreted: $ => /(.|\s)*/,

    statement_list: $ => $.statements,

    statements: $ =>
      choice(
        seq(repeat1(choice($.statement_, $.empty_statement)), optional($.statement)),
        $.statement
      ),

    begin_block: $ => seq('BEGIN', '{', optional($.statements), '}'),
    end_block: $ => seq('END', '{', optional($.statements), '}'),

    statement_: $ => seq($.statement, $._terminator),

    statement: $ =>
      choice(
        $.undef,
        $.alias,
        $.if_modifier,
        $.unless_modifier,
        $.while_modifier,
        $.until_modifier,
        $.rescue_modifier,
        $.begin_block,
        $.end_block,
        $.expression_
      ),

    method: $ => seq('def', $.method_rest),

    singleton_method: $ =>
      seq(
        'def',
        seq(
          choice(field('object', $._variable), seq('(', field('object', $.arg), ')')),
          choice('.', '::')
        ),
        $.method_rest
      ),

    method_rest: $ =>
      seq(
        alias($._method_name, $.identifier),
        choice(
          prec.dynamic(
            1,
            seq(alias($.parameter_list_block, $.parameter_list_optional), optional($.terminator))
          ),
          $.bare_parameters_with_terminator
        ),
        $.body_statement
      ),

    parameter_list_block: $ =>
      seq('(', optional_with_placeholder('parameter_list', commaSep($.parameter)), ')'),

    bare_parameters_with_terminator: $ =>
      seq(
        optional_with_placeholder(
          'parameter_list_optional',
          alias($.bare_parameters, $.parameter_list)
        ),
        $.terminator
      ),

    bare_parameters: $ =>
      seq(alias($._simple_formal_parameter, $.parameter), repeat(seq(',', $.parameter))),

    block_parameters: $ =>
      seq(
        '|',
        seq(commaSep($.parameter), optional(',')),
        optional(seq(';', sep1($.identifier, ','))), // Block shadow args e.g. {|; a, b| ...}
        '|'
      ),

    parameter: $ =>
      choice($._simple_formal_parameter, alias($.parameter_list_block, $.destructured_parameter)),

    _simple_formal_parameter: $ =>
      choice(
        $.identifier,
        $.splat_parameter,
        $.hash_splat_parameter,
        $.block_parameter,
        $.keyword_parameter,
        $.optional_parameter
      ),

    splat_parameter: $ => seq('*', field('name', optional($.identifier))),
    hash_splat_parameter: $ => seq('**', field('name', optional($.identifier))),
    block_parameter: $ => seq('&', field('name', $.identifier)),
    keyword_parameter: $ =>
      prec.right(
        PREC.BITWISE_OR + 1,
        seq(field('name', $.identifier), token.immediate(':'), field('value', optional($.arg)))
      ),
    optional_parameter: $ =>
      prec(PREC.BITWISE_OR + 1, seq(field('name', $.identifier), '=', field('value', $.arg))),

    class: $ =>
      seq(
        'class',
        field('identifier', choice($.constant, $.scope_resolution)),
        optional_with_placeholder('extends_optional', $.extends_optional),
        $.terminator,
        alias($.class_body_statement, $.body_statement)
      ),

    extends_optional: $ => seq('<', alias($.expression_, $.extends_type)),

    singleton_class: $ =>
      seq(
        'class',
        alias($._singleton_class_left_angle_left_langle, '<<'),
        field('value', $.arg),
        $._terminator,
        $.body_statement
      ),

    module: $ =>
      seq(
        'module',
        field('name', choice($.constant, $.scope_resolution)),
        choice(seq($._terminator, $.body_statement), 'end')
      ),

    return_command: $ => prec.left(seq('return', alias($.command_argument_list, $.argument_list))),
    yield_command: $ => prec.left(seq('yield', alias($.command_argument_list, $.argument_list))),
    break_command: $ => prec.left(seq('break', alias($.command_argument_list, $.argument_list))),
    next_command: $ => prec.left(seq('next', alias($.command_argument_list, $.argument_list))),
    return: $ =>
      prec.left(
        seq(
          'return',
          optional_with_placeholder('return_value_optional', alias($.argument_list, $.return_value))
        )
      ),
    yield: $ => prec.left(seq('yield', optional($.argument_list))),
    break: $ => prec.left(seq('break', optional($.argument_list))),
    next: $ => prec.left(seq('next', optional($.argument_list))),
    redo: $ => prec.left(seq('redo', optional($.argument_list))),
    retry: $ => prec.left(seq('retry', optional($.argument_list))),

    if_modifier: $ =>
      prec(PREC.RESCUE, seq(field('body', $.statement), 'if', field('condition', $.expression_))),

    unless_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(field('body', $.statement), 'unless', field('condition', $.expression_))
      ),

    while_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(field('body', $.statement), 'while', field('condition', $.expression_))
      ),

    until_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(field('body', $.statement), 'until', field('condition', $.expression_))
      ),

    rescue_modifier: $ =>
      prec(PREC.RESCUE, seq(field('body', $.statement), 'rescue', field('handler', $.expression_))),

    while: $ => $.while_clause,

    while_clause: $ => seq('while', alias($.statement, $.condition), field('body', $.do)),

    until: $ => $.until_clause,

    until_clause: $ => seq('until', alias($.statement, $.condition), field('body', $.do)),

    for: $ => $.for_each_clause,

    for_each_clause: $ =>
      seq(
        'for',
        field('block_iterator', choice($._lhs, $.left_assignment_list)),
        field('value', $.in),
        $.do
      ),

    in: $ => seq('in', alias($.arg, $.block_collection)),
    do: $ =>
      seq(
        choice('do', $.terminator),
        optional_with_placeholder('statement_list', $.statement_list),
        'end'
      ),

    case: $ =>
      seq(
        'case',
        field('value', optional($.statement)),
        optional($._terminator),
        repeat($.when),
        optional($.else_clause),
        'end'
      ),

    when: $ =>
      seq(
        'when',
        commaSep1(field('pattern', $.pattern)),
        choice($._terminator, field('body', $.then))
      ),

    pattern: $ => choice($.arg, $.splat_argument),

    if: $ =>
      seq(
        $.if_clause,
        optional_with_placeholder('else_if_clause_list', $.else_if_clause_list),
        optional_with_placeholder('else_clause_optional', $.else_clause),
        'end'
      ),

    else_if_clause_list: $ => repeat1($.else_if_clause),

    if_clause: $ => seq('if', field('condition', $.statement), $.then),

    unless: $ =>
      seq(
        'unless',
        field('condition', $.statement),
        choice($._terminator, $.then),
        field('alternative', optional(choice($.else_clause, $.else_if_clause))),
        'end'
      ),

    else_if_clause: $ => seq('elsif', field('condition', $.statement), $.then),

    else_clause: $ =>
      seq(
        'else',
        optional($.terminator),
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    then: $ =>
      choice(
        $.then_postfix,
        seq($.terminator, optional_with_placeholder('statement_list', $.statement_list))
      ),

    then_postfix: $ =>
      seq(
        optional($.terminator),
        'then',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    begin: $ =>
      seq(
        $.begin_clause,
        optional_with_placeholder('rescue_else_ensure_list', $.rescue_else_ensure_list),
        'end'
      ),

    begin_clause: $ =>
      seq(
        'begin',
        optional($.terminator),
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    ensure_clause: $ =>
      seq('ensure', optional_with_placeholder('statement_list', $.statement_list)),

    rescue_clause: $ =>
      seq(
        'rescue',
        optional_with_placeholder('rescue_parameter_optional', $.rescue_parameter),
        $.then
      ),

    rescue_parameter: $ =>
      choice($.exceptions, $.exception_variable, seq($.exceptions, $.exception_variable)),

    exceptions: $ => commaSep1(choice($.arg, $.splat_argument)),

    exception_variable: $ => seq('=>', $._lhs),

    body_end_: $ =>
      seq(optional_with_placeholder('rescue_else_ensure_list', $.rescue_else_ensure_list), 'end'),

    rescue_else_ensure_list: $ => repeat1(choice($.rescue_clause, $.else_clause, $.ensure_clause)),

    body_statement: $ =>
      seq(
        optional_with_placeholder('statement_list', $.statement_list),
        optional_with_placeholder('rescue_else_ensure_list', $.rescue_else_ensure_list),
        'end'
      ),

    class_body_statement: $ =>
      seq(
        optional_with_placeholder('class_member_list', $.class_member_list),
        optional_with_placeholder('rescue_else_ensure_list', $.rescue_else_ensure_list),
        'end'
      ),

    class_member_list: $ => $.members,

    members: $ =>
      choice(
        seq(repeat1(choice(seq($.member, $._terminator), $.empty_statement)), optional($.member)),
        $.member
      ),

    member: $ =>
      choice(
        $.undef,
        $.alias,
        $.if_modifier,
        $.unless_modifier,
        $.while_modifier,
        $.until_modifier,
        $.rescue_modifier,
        $.begin_block,
        $.end_block,
        $.expression_
      ),

    // Method calls without parentheses (aka "command calls") are only allowed
    // in certain positions, like the top-level of a statement, the condition
    // of a postfix control-flow operator like `if`, or as the value of a
    // control-flow statement like `return`. In many other places, they're not
    // allowed.
    //
    // Because of this distinction, a lot of rules have two variants: the
    // normal variant, which can appear anywhere that an expression is valid,
    // and the "command" varaint, which is only valid in a more limited set of
    // positions, because it can contain "command calls".
    //
    // The `_expression` rule can appear in relatively few places, but can
    // contain command calls. The `_arg` rule can appear in many more places,
    // but cannot contain command calls (unless they are wrapped in parens).
    // This naming convention is based on Ruby's standard grammar.
    expression_: $ =>
      choice(
        alias($.command_binary, $.binary),
        alias($.command_unary, $.unary),
        alias($.command_assignment, $.assignment),
        alias($.command_operator_assignment, $.operator_assignment),
        $.command_call,
        $.command_call_with_block,
        prec.left(alias($._chained_command_call, $.chained_command_call)),
        alias($.return_command, $.return),
        alias($.yield_command, $.yield),
        alias($.break_command, $.break),
        alias($.next_command, $.next),
        $.arg
      ),

    arg: $ =>
      choice(
        $.primary,
        $.assignment,
        $.operator_assignment,
        $.conditional,
        $.range,
        $.binary,
        $.unary
      ),

    primary: $ =>
      choice(
        $.parenthesized_statements,
        $._lhs,
        $.list,
        $.string_array,
        $.symbol_array,
        $.dictionary,
        $.subshell,
        $.simple_symbol,
        $.delimited_symbol,
        $.integer,
        $.float,
        $.complex,
        $.rational,
        $.string,
        $.character,
        $.chained_string,
        $.regex,
        $.lambda,
        $.method,
        $.singleton_method,
        $.class,
        $.singleton_class,
        $.module,
        $.begin,
        $.while,
        $.until,
        $.if,
        $.unless,
        $.for,
        $.case,
        $.return,
        $.yield,
        $.break,
        $.next,
        $.redo,
        $.retry,
        $.parenthesized_unary,
        $.unary_literal,
        $.heredoc_beginning
      ),

    parenthesized_statements: $ => seq('(', optional($.statements), ')'),

    element_reference: $ =>
      prec.left(
        1,
        seq(
          field('object', $.primary),
          alias($._element_reference_bracket, '['),
          optional($.argument_list),
          ']'
        )
      ),

    scope_resolution: $ =>
      prec.left(
        1,
        seq(
          choice('::', seq(field('scope', $.primary), token.immediate('::'))),
          field('name', choice($.identifier, $.constant))
        )
      ),

    _call_expression: $ =>
      prec.left(
        PREC.CALL,
        seq(
          field('receiver', $.primary),
          choice('.', '&.'),
          field('method_identifier', choice($.identifier, $.operator, $.constant, $.argument_list))
        )
      ),

    command_call: $ =>
      seq(
        choice(
          $._call_expression,
          $._chained_command_call,
          field('method_identifier', choice($._variable, $.scope_resolution))
        ),
        field('arguments', alias($.command_argument_list, $.argument_list))
      ),

    command_call_with_block: $ => {
      const receiver = choice(
        $._call_expression,
        field('method_identifier', choice($._variable, $.scope_resolution))
      )
      const arguments = field('arguments', alias($.command_argument_list, $.argument_list))
      const block = field('block', $.enclosed_body_)
      const doBlock = field('block', $.do_block)
      return choice(
        seq(receiver, prec(PREC.CURLY_BLOCK, seq(arguments, block))),
        seq(receiver, prec(PREC.DO_BLOCK, seq(arguments, doBlock)))
      )
    },

    _chained_command_call: $ =>
      seq(
        field('receiver', alias($.command_call_with_block, $.call)),
        choice('.', '&.'),
        field('method_identifier', choice($.identifier, $.operator, $.constant, $.argument_list))
      ),

    call: $ => {
      const receiver = choice(
        $._call_expression,
        field('method_identifier', choice($._variable, $.scope_resolution))
      )
      return choice(
        seq(receiver, $.arguments),
        seq(receiver, prec(PREC.CURLY_BLOCK, seq($.arguments, $.enclosed_body_))),
        seq(receiver, prec(PREC.DO_BLOCK, seq($.arguments, $.do_block))),
        prec(PREC.CURLY_BLOCK, seq(receiver, $.enclosed_body_)),
        prec(PREC.DO_BLOCK, seq(receiver, $.do_block))
      )
    },

    command_argument_list: $ => prec.right(commaSep1($.argument)),

    arguments: $ =>
      prec.right(
        seq(token.immediate('('), optional_with_placeholder('argument_list', $.argument_list), ')')
      ),

    argument_list: $ => prec.right(seq(commaSep1($.argument), optional(','))),

    inner_list: $ => prec.right(seq(commaSep1(alias($.argument, $.list_element)), optional(','))),

    argument: $ =>
      prec.left(
        choice($.expression_, $.splat_argument, $.hash_splat_argument, $.block_argument, $.pair)
      ),

    splat_argument: $ => seq(alias($._splat_star, '*'), $.arg),
    hash_splat_argument: $ => seq(alias($._hash_splat_star_star, '**'), $.arg),
    block_argument: $ => seq(alias($._block_ampersand, '&'), $.arg),

    do_block: $ =>
      seq(
        'do',
        optional($._terminator),
        optional(seq(field('parameters', $.block_parameters), optional($._terminator))),
        $.body_statement
      ),

    enclosed_body_: $ =>
      prec(
        PREC.CURLY_BLOCK,
        seq(
          '{',
          field('parameters', optional($.block_parameters)),
          optional_with_placeholder('statement_list_', alias($.statement_list, $.statement_list_)),
          '}'
        )
      ),

    assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        choice(
          seq(
            field('assignment_variable', choice($._lhs, $.left_assignment_list)),
            '=',
            field('assignment_value', choice($.arg, $.splat_argument, $.right_assignment_list))
          )
        )
      ),

    command_assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        choice(
          seq(
            field('left', choice($._lhs, $.left_assignment_list)),
            '=',
            field('right', $.expression_)
          )
        )
      ),

    operator_assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        seq(
          field('left', $._lhs),
          field(
            'operator',
            choice(
              '+=',
              '-=',
              '*=',
              '**=',
              '/=',
              '||=',
              '|=',
              '&&=',
              '&=',
              '%=',
              '>>=',
              '<<=',
              '^='
            )
          ),
          field('right', $.arg)
        )
      ),

    command_operator_assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        seq(
          field('left', $._lhs),
          field(
            'operator',
            choice(
              '+=',
              '-=',
              '*=',
              '**=',
              '/=',
              '||=',
              '|=',
              '&&=',
              '&=',
              '%=',
              '>>=',
              '<<=',
              '^='
            )
          ),
          field('right', $.expression_)
        )
      ),

    conditional: $ =>
      prec.right(
        PREC.CONDITIONAL,
        seq(
          field('condition', $.arg),
          '?',
          field('consequence', $.arg),
          ':',
          field('alternative', $.arg)
        )
      ),

    range: $ => {
      const begin = field('begin_', $.arg)
      const end = field('end', $.arg)
      const operator = field('operator', choice('..', '...'))
      return prec.right(
        PREC.RANGE,
        choice(seq(begin, operator, end), seq(operator, end), seq(begin, operator))
      )
    },

    binary: $ => {
      const operators = [
        [prec.left, PREC.AND, 'and'],
        [prec.left, PREC.OR, 'or'],
        [prec.left, PREC.BOOLEAN_OR, '||'],
        [prec.left, PREC.BOOLEAN_OR, '&&'],
        [prec.left, PREC.SHIFT, choice('<<', '>>')],
        [prec.left, PREC.COMPARISON, choice('<', '<=', '>', '>=')],
        [prec.left, PREC.BITWISE_AND, '&'],
        [prec.left, PREC.BITWISE_OR, choice('^', '|')],
        [prec.left, PREC.ADDITIVE, choice('+', alias($._binary_minus, '-'))],
        [prec.left, PREC.MULTIPLICATIVE, choice('/', '%', alias($._binary_star, '*'))],
        [prec.right, PREC.RELATIONAL, choice('==', '!=', '===', '<=>', '=~', '!~')],
        [prec.right, PREC.EXPONENTIAL, alias($._binary_star_star, '**')],
      ]

      return choice(
        ...operators.map(([fn, precedence, operator]) =>
          fn(
            precedence,
            seq(field('left', $.arg), field('operator', operator), field('right', $.arg))
          )
        )
      )
    },

    command_binary: $ =>
      prec.left(
        seq(
          field('left', $.expression_),
          field('operator', choice('or', 'and')),
          field('right', $.expression_)
        )
      ),

    unary: $ => {
      const operators = [
        [prec, PREC.DEFINED, 'defined?'],
        [prec.right, PREC.NOT, 'not'],
        [prec.right, PREC.UNARY_MINUS, choice(alias($._unary_minus, '-'), '+')],
        [prec.right, PREC.COMPLEMENT, choice('!', '~')],
      ]
      return choice(
        ...operators.map(([fn, precedence, operator]) =>
          fn(precedence, seq(field('operator', operator), field('operand', $.arg)))
        )
      )
    },

    command_unary: $ => {
      const operators = [
        [prec, PREC.DEFINED, 'defined?'],
        [prec.right, PREC.NOT, 'not'],
        [prec.right, PREC.UNARY_MINUS, choice(alias($._unary_minus, '-'), '+')],
        [prec.right, PREC.COMPLEMENT, choice('!', '~')],
      ]
      return choice(
        ...operators.map(([fn, precedence, operator]) =>
          fn(precedence, seq(field('operator', operator), field('operand', $.expression_)))
        )
      )
    },

    parenthesized_unary: $ =>
      prec(
        PREC.CALL,
        seq(
          field('operator', choice('defined?', 'not')),
          field('operand', $.parenthesized_statements)
        )
      ),

    unary_literal: $ =>
      prec.right(
        PREC.UNARY_MINUS,
        seq(
          field('operator', choice(alias($._unary_minus, '-'), '+')),
          field('operand', choice($.integer, $.float))
        )
      ),

    right_assignment_list: $ => prec(-1, commaSep1(choice($.arg, $.splat_argument))),

    left_assignment_list: $ => $._mlhs,
    _mlhs: $ =>
      prec.left(
        -1,
        seq(
          commaSep1(choice($._lhs, $.rest_assignment, $.destructured_left_assignment)),
          optional(',')
        )
      ),
    destructured_left_assignment: $ => prec(-1, seq('(', $._mlhs, ')')),

    rest_assignment: $ => prec(-1, seq('*', optional($._lhs))),

    _lhs: $ =>
      prec.left(
        choice(
          $._variable,
          $.true,
          $.false,
          $.nil_,
          $.scope_resolution,
          $.element_reference,
          alias($._call_expression, $.call),
          $.call
        )
      ),

    _variable: $ =>
      prec.right(
        choice(
          $.self,
          $.super,
          $.instance_variable,
          $.class_variable,
          $.global_variable,
          $.identifier,
          $.constant
        )
      ),

    operator: $ =>
      choice(
        '..',
        '|',
        '^',
        '&',
        '<=>',
        '==',
        '===',
        '=~',
        '>',
        '>=',
        '<',
        '<=',
        '+',
        '-',
        '*',
        '/',
        '%',
        '!',
        '!~',
        '**',
        '<<',
        '>>',
        '~',
        '+@',
        '-@',
        '[]',
        '[]=',
        '`'
      ),

    _method_name: $ =>
      choice(
        $.identifier,
        $.constant,
        $.setter,
        $.simple_symbol,
        $.delimited_symbol,
        $.operator,
        $.instance_variable,
        $.class_variable,
        $.global_variable
      ),
    setter: $ => seq(field('name', $.identifier), '='),

    undef: $ => seq('undef', commaSep1($._method_name)),
    alias: $ => seq('alias', field('name', $._method_name), field('alias', $._method_name)),

    comment: $ =>
      token(
        prec(
          PREC.COMMENT,
          choice(
            seq('#', /.*/),
            seq(/=begin.*\r?\n/, repeat(choice(/[^=]/, /=[^e]/, /=e[^n]/, /=en[^d]/)), /=end/)
          )
        )
      ),

    integer: $ =>
      /0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])*/,

    float: $ => /\d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*)?/,
    complex: $ => /(\d+)?(\+|-)?(\d+)i/,
    rational: $ => seq(choice($.integer, $.float), 'r'),
    super: $ => 'super',
    self: $ => 'self',
    true: $ => token(choice('true', 'TRUE')),
    false: $ => token(choice('false', 'FALSE')),
    nil_: $ => token(choice('nil', 'NIL')),

    constant: $ => token(seq(/[A-Z]/, IDENTIFIER_CHARS, /(\?|\!)?/)),
    identifier: $ => token(seq(LOWER_ALPHA_CHAR, IDENTIFIER_CHARS, /(\?|\!)?/)),
    instance_variable: $ => token(seq('@', ALPHA_CHAR, IDENTIFIER_CHARS)),
    class_variable: $ => token(seq('@@', ALPHA_CHAR, IDENTIFIER_CHARS)),

    global_variable: $ => /\$-?(([!@&`'+~=/\\,;.<>*$?:"])|([0-9]*)|([a-zA-Z_][a-zA-Z0-9_]*))/,

    chained_string: $ => seq($.string, repeat1($.string)),

    character: $ => /\?(\\\S({[0-9A-Fa-f]*}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S)/,

    interpolation: $ => seq('#{', optional($.statements), '}'),

    string: $ =>
      seq(alias($._string_start, '"'), optional($._literal_contents), alias($._string_end, '"')),

    subshell: $ =>
      seq(alias($._subshell_start, '`'), optional($._literal_contents), alias($._string_end, '`')),

    string_array: $ =>
      seq(
        alias($._string_array_start, '%w('),
        optional(/\s+/),
        sep(alias($._literal_contents, $.bare_string), /\s+/),
        optional(/\s+/),
        alias($._string_end, ')')
      ),

    symbol_array: $ =>
      seq(
        alias($._symbol_array_start, '%i('),
        optional(/\s+/),
        sep(alias($._literal_contents, $.bare_symbol), /\s+/),
        optional(/\s+/),
        alias($._string_end, ')')
      ),

    delimited_symbol: $ =>
      seq(alias($._symbol_start, ':"'), optional($._literal_contents), alias($._string_end, '"')),

    regex: $ =>
      seq(alias($._regex_start, '/'), optional($._literal_contents), alias($._string_end, '/')),

    heredoc_body: $ =>
      seq(
        $._heredoc_body_start,
        repeat(choice($.heredoc_content, $.interpolation, $.escape_sequence)),
        $.heredoc_end
      ),

    _literal_contents: $ => repeat1(choice($.string_content, $.interpolation, $.escape_sequence)),

    // https://ruby-doc.org/core-2.5.0/doc/syntax/literals_rdoc.html#label-Strings
    escape_sequence: $ =>
      token(
        seq(
          '\\',
          choice(
            /[^ux0-7]/, // single character
            /x[0-9a-fA-F]{1,2}/, // hex code
            /[0-7]{1,3}/, // octal
            /u[0-9a-fA-F]{4}/, // single unicode
            /u{[0-9a-fA-F ]+}/ // multiple unicode
          )
        )
      ),

    list: $ => seq('[', optional_with_placeholder('inner_list', $.inner_list), ']'),

    dictionary: $ =>
      seq('{', optional(seq(commaSep1(choice($.pair, $.hash_splat_argument)), optional(','))), '}'),

    pair: $ =>
      choice(
        seq(field('key', $.arg), '=>', field('value', $.arg)),
        seq(
          field(
            'key',
            choice(
              $.hash_key_symbol,
              alias($.identifier, $.hash_key_symbol),
              alias($.constant, $.hash_key_symbol),
              $.string
            )
          ),
          token.immediate(':'),
          field('value', $.arg)
        )
      ),

    lambda: $ =>
      seq(
        '->',
        choice(
          alias($.parameter_list_block, $.lambda_parameters),
          optional_with_placeholder('paremeter_list', $.bare_parameters)
        ),
        choice($.enclosed_body_, $.do_block)
      ),

    empty_statement: $ => prec(-1, ';'),

    _terminator: $ => choice($._line_break, ';'),
    terminator: $ => $._terminator,
  },
})

function sep(rule, separator) {
  return optional(sep1(rule, separator))
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)))
}

function commaSep1(rule) {
  return sep1(rule, ',')
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}

function optional_with_placeholder(field_name, rule) {
  return choice(field(field_name, rule), field(field_name, blank()))
}
