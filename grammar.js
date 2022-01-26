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
const LOWER_ALPHA_CHAR =
  /[^\x00-\x1F\sA-Z0-9:;`"'@$#.,|^&<=>+\-*/\\%?!~()\[\]{}]/
const ALPHA_CHAR = /[^\x00-\x1F\s0-9:;`"'@$#.,|^&<=>+\-*/\\%?!~()\[\]{}]/

module.exports = grammar({
  name: 'ruby',

  externals: $ => [
    $.line_break,

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

  supertypes: $ => [],

  rules: {
    program: $ =>
      seq(
        optional_with_placeholder('statement_list', $.statement_list),
        optional(seq('__END__', $.line_break, $.uninterpreted))
      ),

    uninterpreted: $ => /(.|\s)*/,

    enclosed_body: $ => $.statement_list,

    statement_list: $ =>
      choice(
        seq(
          repeat1(choice(seq($.statement, $.terminator_), $.empty_statement)),
          optional($.statement)
        ),
        $.statement
      ),

    begin_block: $ => seq('BEGIN', '{', optional($.statement_list), '}'),
    end_block: $ => seq('END', '{', optional($.statement_list), '}'),

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

    method: $ => seq('def', $.method_rest_),

    singleton_method: $ =>
      seq(
        'def',
        seq(choice($.variable_, seq('(', $.arg, ')')), choice('.', '::')),
        $.method_rest_
      ),

    method_rest_: $ =>
      seq(
        alias($.method_name_, $.identifier),
        choice(
          seq(
            field('parameters', alias($.parameters, $.method_parameters)),
            optional($.terminator_)
          ),
          seq(
            optional(
              field('parameters', alias($.bare_parameters, $.method_parameters))
            ),
            $.terminator_
          )
        ),
        $._body_statement
      ),

    parameters: $ => seq('(', commaSep($.parameter), ')'),

    bare_parameters: $ =>
      seq($.simple_formal_parameter_, repeat(seq(',', $.parameter))),

    block_parameters: $ =>
      seq(
        '|',
        seq(commaSep($.parameter), optional(',')),
        optional(seq(';', sep1($.identifier, ','))), // Block shadow args e.g. {|; a, b| ...}
        '|'
      ),

    parameter: $ =>
      choice(
        $.simple_formal_parameter_,
        alias($.parameters, $.destructured_parameter)
      ),

    simple_formal_parameter_: $ =>
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
        seq(
          field('name', $.identifier),
          token.immediate(':'),
          field('value', optional($.arg))
        )
      ),
    optional_parameter: $ =>
      prec(
        PREC.BITWISE_OR + 1,
        seq(field('name', $.identifier), '=', field('value', $.arg))
      ),

    class: $ =>
      seq(
        'class',
        field('name', choice($.constant, $.scope_resolution)),
        field('superclass', optional($.superclass)),
        $.terminator_,
        $._body_statement
      ),

    superclass: $ => seq('<', $.expression_),

    singleton_class: $ =>
      seq(
        'class',
        alias($._singleton_class_left_angle_left_langle, '<<'),
        field('value', $.arg),
        $.terminator_,
        $._body_statement
      ),

    module: $ =>
      seq(
        'module',
        field('name', choice($.constant, $.scope_resolution)),
        choice(seq($.terminator_, $._body_statement), 'end')
      ),

    return_command: $ =>
      prec.left(seq('return', alias($.command_argument_list, $.arguments))),
    yield_command: $ =>
      prec.left(seq('yield', alias($.command_argument_list, $.arguments))),
    break_command: $ =>
      prec.left(seq('break', alias($.command_argument_list, $.arguments))),
    next_command: $ =>
      prec.left(seq('next', alias($.command_argument_list, $.arguments))),
    return: $ => prec.left(seq('return', optional($.arguments))),
    yield: $ => prec.left(seq('yield', optional($.arguments))),
    break: $ => prec.left(seq('break', optional($.arguments))),
    next: $ => prec.left(seq('next', optional($.arguments))),
    redo: $ => prec.left(seq('redo', optional($.arguments))),
    retry: $ => prec.left(seq('retry', optional($.arguments))),

    if_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(field('body', $.statement), 'if', field('condition', $.expression_))
      ),

    unless_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(
          field('body', $.statement),
          'unless',
          field('condition', $.expression_)
        )
      ),

    while_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(
          field('body', $.statement),
          'while',
          field('condition', $.expression_)
        )
      ),

    until_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(
          field('body', $.statement),
          'until',
          field('condition', $.expression_)
        )
      ),

    rescue_modifier: $ =>
      prec(
        PREC.RESCUE,
        seq(
          field('body', $.statement),
          'rescue',
          field('handler', $.expression_)
        )
      ),

    while: $ =>
      seq('while', field('condition', $.statement), field('body', $.do)),

    until: $ =>
      seq('until', field('condition', $.statement), field('body', $.do)),

    for: $ =>
      seq(
        'for',
        field('pattern', choice($.lhs_, $.left_assignment_list)),
        field('value', $.in),
        field('body', $.do)
      ),

    in: $ => seq('in', $.arg),
    do: $ =>
      seq(
        choice('do', $.terminator_),
        optional_with_placeholder('statement_list', $.statement_list),
        'end'
      ),

    case: $ =>
      seq(
        'case',
        field('value', optional($.statement)),
        optional($.terminator_),
        repeat($.when),
        optional($.else),
        'end'
      ),

    when: $ =>
      seq(
        'when',
        commaSep1(field('pattern', $.pattern)),
        choice($.terminator_, field('body', $.then))
      ),

    pattern: $ => choice($.arg, $.splat_argument),

    if: $ =>
      seq(
        'if',
        field('condition', $.statement),
        choice($.terminator_, field('consequence', $.then)),
        field('alternative', optional(choice($.else, $.elsif))),
        'end'
      ),

    unless: $ =>
      seq(
        'unless',
        field('condition', $.statement),
        choice($.terminator_, field('consequence', $.then)),
        field('alternative', optional(choice($.else, $.elsif))),
        'end'
      ),

    elsif: $ =>
      seq(
        'elsif',
        field('condition', $.statement),
        choice($.terminator_, field('consequence', $.then)),
        field('alternative', optional(choice($.else, $.elsif)))
      ),

    else: $ =>
      seq(
        'else',
        optional($.terminator_),
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    then_implicit_: $ => seq($.terminator_, $.statement_list),

    then_separated_: $ =>
      seq(
        optional($.terminator_),
        'then',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    then: $ => choice($.then_implicit_, $.then_separated_),

    begin: $ => seq('begin', optional($.terminator_), $._body_statement),

    ensure: $ =>
      seq(
        'ensure',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    rescue: $ =>
      seq(
        'rescue',
        field('exceptions', optional($.exceptions)),
        field('variable', optional($.exception_variable)),
        choice($.terminator_, field('body', $.then))
      ),

    exceptions: $ => commaSep1(choice($.arg, $.splat_argument)),

    exception_variable: $ => seq('=>', $.lhs_),

    _body_statement: $ =>
      seq(
        optional_with_placeholder('statement_list', $.statement_list),
        repeat(choice($.rescue, $.else, $.ensure)),
        'end'
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
        alias($.command_call, $.call),
        alias($.command_call_with_block, $.call),
        prec.left(alias($._chained_command_call, $.call)),
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
        $.lhs_,
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
        alias($.parenthesized_unary, $.unary),
        alias($.unary_literal, $.unary),
        $.heredoc_beginning
      ),

    parenthesized_statements: $ =>
      seq(
        '(',
        optional_with_placeholder('statement_list', $.statement_list),
        ')'
      ),

    element_reference: $ =>
      prec.left(
        1,
        seq(
          field('object', $.primary),
          alias($._element_reference_bracket, '['),
          optional($._argument_list_with_trailing_comma),
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

    call_expression_: $ =>
      prec.left(
        PREC.CALL,
        seq(
          field('receiver', $.primary),
          choice('.', '&.'),
          field(
            'method',
            choice($.identifier, $.operator, $.constant, $.arguments)
          )
        )
      ),

    command_call: $ =>
      seq(
        choice(
          $.call_expression_,
          $._chained_command_call,
          field('method', choice($.variable_, $.scope_resolution))
        ),
        field('arguments', alias($.command_argument_list, $.arguments))
      ),

    command_call_with_block: $ => {
      const receiver = choice(
        $.call_expression_,
        field('method', choice($.variable_, $.scope_resolution))
      )
      const arguments = field(
        'arguments',
        alias($.command_argument_list, $.arguments)
      )
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
        field(
          'method',
          choice($.identifier, $.operator, $.constant, $.arguments)
        )
      ),

    call: $ => {
      const receiver = choice(
        $.call_expression_,
        field('method', choice($.variable_, $.scope_resolution))
      )
      const arguments = field('arguments', $.arguments)
      const block = field('block', $.enclosed_body_)
      const doBlock = field('block', $.do_block)
      return choice(
        seq(receiver, arguments),
        seq(receiver, prec(PREC.CURLY_BLOCK, seq(arguments, block))),
        seq(receiver, prec(PREC.DO_BLOCK, seq(arguments, doBlock))),
        prec(PREC.CURLY_BLOCK, seq(receiver, block)),
        prec(PREC.DO_BLOCK, seq(receiver, doBlock))
      )
    },

    command_argument_list: $ => prec.right(commaSep1($.argument)),

    arguments: $ =>
      prec.right(
        seq(
          token.immediate('('),
          optional($._argument_list_with_trailing_comma),
          ')'
        )
      ),

    _argument_list_with_trailing_comma: $ =>
      prec.right(seq(commaSep1($.argument), optional(','))),

    argument: $ =>
      prec.left(
        choice(
          $.expression_,
          $.splat_argument,
          $.hash_splat_argument,
          $.block_argument,
          $.pair
        )
      ),

    splat_argument: $ => seq(alias($._splat_star, '*'), $.arg),
    hash_splat_argument: $ => seq(alias($._hash_splat_star_star, '**'), $.arg),
    block_argument: $ => seq(alias($._block_ampersand, '&'), $.arg),

    do_block: $ =>
      seq(
        'do',
        optional($.terminator_),
        optional(
          seq(field('parameters', $.block_parameters), optional($.terminator_))
        ),
        $._body_statement
      ),

    enclosed_body_: $ =>
      prec(
        PREC.CURLY_BLOCK,
        seq(
          '{',
          field('parameters', optional($.block_parameters)),
          optional_with_placeholder('statement_list', $.statement_list),
          '}'
        )
      ),

    assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        choice(
          seq(
            field('left', choice($.lhs_, $.left_assignment_list)),
            '=',
            field(
              'right',
              choice($.arg, $.splat_argument, $.right_assignment_list)
            )
          )
        )
      ),

    command_assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        choice(
          seq(
            field('left', choice($.lhs_, $.left_assignment_list)),
            '=',
            field('right', $.expression_)
          )
        )
      ),

    operator_assignment: $ =>
      prec.right(
        PREC.ASSIGN,
        seq(
          field('left', $.lhs_),
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
          field('left', $.lhs_),
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
      const begin = field('begin', $.arg)
      const end = field('end', $.arg)
      const operator = field('operator', choice('..', '...'))
      return prec.right(
        PREC.RANGE,
        choice(
          seq(begin, operator, end),
          seq(operator, end),
          seq(begin, operator)
        )
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
        [
          prec.left,
          PREC.MULTIPLICATIVE,
          choice('/', '%', alias($._binary_star, '*')),
        ],
        [
          prec.right,
          PREC.RELATIONAL,
          choice('==', '!=', '===', '<=>', '=~', '!~'),
        ],
        [prec.right, PREC.EXPONENTIAL, alias($._binary_star_star, '**')],
      ]

      return choice(
        ...operators.map(([fn, precedence, operator]) =>
          fn(
            precedence,
            seq(
              field('left', $.arg),
              field('operator', operator),
              field('right', $.arg)
            )
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
          fn(
            precedence,
            seq(field('operator', operator), field('operand', $.arg))
          )
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
          fn(
            precedence,
            seq(field('operator', operator), field('operand', $.expression_))
          )
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

    right_assignment_list: $ =>
      prec(-1, commaSep1(choice($.arg, $.splat_argument))),

    left_assignment_list: $ => $._mlhs,
    _mlhs: $ =>
      prec.left(
        -1,
        seq(
          commaSep1(
            choice($.lhs_, $.rest_assignment, $.destructured_left_assignment)
          ),
          optional(',')
        )
      ),
    destructured_left_assignment: $ => prec(-1, seq('(', $._mlhs, ')')),

    rest_assignment: $ => prec(-1, seq('*', optional($.lhs_))),

    lhs_: $ =>
      prec.left(
        choice(
          $.variable_,
          $.true,
          $.false,
          $.nil_,
          $.scope_resolution,
          $.element_reference,
          alias($.call_expression_, $.call),
          $.call
        )
      ),

    variable_: $ =>
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

    method_name_: $ =>
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

    undef: $ => seq('undef', commaSep1($.method_name_)),
    alias: $ =>
      seq(
        'alias',
        field('name', $.method_name_),
        field('alias', $.method_name_)
      ),

    comment: $ =>
      token(
        prec(
          PREC.COMMENT,
          choice(
            seq('#', /.*/),
            seq(
              /=begin.*\r?\n/,
              repeat(choice(/[^=]/, /=[^e]/, /=e[^n]/, /=en[^d]/)),
              /=end/
            )
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

    global_variable: $ =>
      /\$-?(([!@&`'+~=/\\,;.<>*$?:"])|([0-9]*)|([a-zA-Z_][a-zA-Z0-9_]*))/,

    chained_string: $ => seq($.string, repeat1($.string)),

    character: $ => /\?(\\\S({[0-9A-Fa-f]*}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S)/,

    interpolation: $ => seq('#{', optional($.statement_list), '}'),

    string: $ =>
      seq(
        alias($._string_start, '"'),
        optional($._literal_contents),
        alias($._string_end, '"')
      ),

    subshell: $ =>
      seq(
        alias($._subshell_start, '`'),
        optional($._literal_contents),
        alias($._string_end, '`')
      ),

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
      seq(
        alias($._symbol_start, ':"'),
        optional($._literal_contents),
        alias($._string_end, '"')
      ),

    regex: $ =>
      seq(
        alias($._regex_start, '/'),
        optional($._literal_contents),
        alias($._string_end, '/')
      ),

    heredoc_body: $ =>
      seq(
        $._heredoc_body_start,
        repeat(choice($.heredoc_content, $.interpolation, $.escape_sequence)),
        $.heredoc_end
      ),

    _literal_contents: $ =>
      repeat1(choice($.string_content, $.interpolation, $.escape_sequence)),

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

    list: $ => seq('[', optional($._argument_list_with_trailing_comma), ']'),

    dictionary: $ =>
      seq(
        '{',
        optional(
          seq(commaSep1(choice($.pair, $.hash_splat_argument)), optional(','))
        ),
        '}'
      ),

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
        field(
          'parameters',
          optional(
            choice(
              alias($.parameters, $.lambda_parameters),
              alias($.bare_parameters, $.lambda_parameters)
            )
          )
        ),
        field('body', choice($.enclosed_body_, $.do_block))
      ),

    empty_statement: $ => prec(-1, ';'),

    terminator_: $ => choice($.line_break, ';'),
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
