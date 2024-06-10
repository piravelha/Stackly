package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type TokenType = int

const (
	TokenTypeInt    = iota
	TokenTypeChar   = iota
	TokenTypeString = iota
	TokenTypeSymbol = iota
	TokenTypeName   = iota
	TokenTypeStruct = iota
	TokenTypeNil    = iota
	TokenTypeQuote  = iota
)

type Location struct {
	File   string
	Line   int
	Column int
}

func (l Location) Get() []interface{} {
	return []interface{}{l.File, l.Line, l.Column}
}

type Token struct {
	Type     TokenType
	Value    string
	Location Location
}

type OpType = int

const (
	OpTypeNoOp       = iota
	OpTypeInclude    = iota
	OpTypeCons       = iota
	OpTypeAssert     = iota
	OpTypePushInt    = iota
	OpTypePushChar   = iota
	OpTypePushNil    = iota
	OpTypePushQuote  = iota
	OpTypeSplice     = iota
	OpTypeSaveSymbol = iota
	OpTypeStruct     = iota
	OpTypeNewStruct  = iota
	OpTypeProp       = iota
	OpTypeAdd        = iota
	OpTypeSub        = iota
	OpTypeMul        = iota
	OpTypeDiv        = iota
	OpTypeMod        = iota
	OpTypeLt         = iota
	OpTypeGt         = iota
	OpTypeEq         = iota
	OpTypeAnd        = iota
	OpTypeOr         = iota
	OpTypeNot        = iota
	OpTypePrint      = iota
	OpTypeShow       = iota
	OpTypeDup        = iota
	OpTypeSwap       = iota
	OpTypeDrop       = iota
	OpTypeOver       = iota
	OpTypeIota       = iota
	OpTypeLen        = iota
	OpTypeHead       = iota
	OpTypeTail       = iota
	OpTypeAppend     = iota
	OpTypeName       = iota
	OpTypePrintType  = iota
	OpTypePrintStack = iota

	OpTypeClearMacroSymbols = iota
)

type Op struct {
	Type  OpType
	Value Token
}

type TreeType = int

const (
	TreeTypeExpression = iota
	TreeTypeStruct     = iota
	TreeTypeNewStruct  = iota
	TreeTypeSymbol     = iota
	TreeTypeThen       = iota
	TreeTypeThenElse   = iota
	TreeTypeWhile      = iota
	TreeTypeMacro      = iota
	TreeTypeQuote      = iota
)

type Tree struct {
	Type  TreeType
	Nodes []interface{}
	Token Token
}

type RawType = int

const (
	RawTypeInt        = iota
	RawTypeChar       = iota
	RawTypeList       = iota
	RawTypeQuote      = iota
	RawTypeStruct     = iota
	RawTypeSymbolType = iota
	RawTypeUndefined  = iota
)

type Type struct {
	Type RawType
	Args []interface{}
}

type TypeStackEntry struct {
	Type  Type
	Token Token
}

type StackEntry struct {
	Value interface{}
	Token Token
}

type ValueStruct struct {
	Name   string
	Values []interface{}
}

func lex(file string, code string) []Token {
	tokens := []Token{}
	line := 1
	col := 1
	ws_regex, _ := regexp.Compile(`^([ \t()]+)`)
	comment_regex, _ := regexp.Compile(`^(;.*)`)
	newline_regex, _ := regexp.Compile(`^(\n)`)
	array_regex, _ := regexp.Compile(`^(\[\])`)
	char_regex, _ := regexp.Compile(`^'([^'])'`)
	string_regex, _ := regexp.Compile(`^"([^"]*)"`)
	num_regex, _ := regexp.Compile(`^(\d+)`)
	symbol_regex, _ := regexp.Compile(`^:([a-z_\-?!@]+)`)
	name_regex, _ := regexp.Compile(`^([^A-Z\s\d'"{}][^\s\d'"{}]*)`)
	struct_regex, _ := regexp.Compile(`^([A-Z][a-zA-Z0-9_]*)`)
	quote_regex, _ := regexp.Compile(`^([{}])`)
	for code != "" {
		if ws_regex.MatchString(code) {
			ws := ws_regex.FindStringSubmatch(code)
			col += len(ws[1])
			code = code[len(ws[1]):]
			continue
		}
		if comment_regex.MatchString(code) {
			comment := comment_regex.FindStringSubmatch(code)
			col += len(comment[1])
			code = code[len(comment[1]):]
			continue
		}
		if newline_regex.MatchString(code) {
			line += 1
			col = 1
			code = code[1:]
			continue
		}
		loc := Location{file, line, col}
		if array_regex.MatchString(code) {
			arr := array_regex.FindStringSubmatch(code)
			col += len(arr[1])
			code = code[len(arr[1]):]
			tok := Token{TokenTypeNil, arr[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if char_regex.MatchString(code) {
			char := char_regex.FindStringSubmatch(code)
			col += len(char[1]) + 2
			code = code[len(char[1])+2:]
			tok := Token{TokenTypeChar, char[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if string_regex.MatchString(code) {
			str := string_regex.FindStringSubmatch(code)
			col += len(str[1]) + 2
			code = code[len(str[1])+2:]
			tok := Token{TokenTypeString, str[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if num_regex.MatchString(code) {
			num := num_regex.FindStringSubmatch(code)
			col += len(num[1])
			code = code[len(num[1]):]
			tok := Token{TokenTypeInt, num[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if quote_regex.MatchString(code) {
			quote := quote_regex.FindStringSubmatch(code)
			col += 1
			code = code[1:]
			tok := Token{TokenTypeQuote, quote[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if symbol_regex.MatchString(code) {
			sym := symbol_regex.FindStringSubmatch(code)
			col += len(sym[1]) + 1
			code = code[len(sym[1])+1:]
			tok := Token{TokenTypeSymbol, sym[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if name_regex.MatchString(code) {
			name := name_regex.FindStringSubmatch(code)
			col += len(name[1])
			code = code[len(name[1]):]
			tok := Token{TokenTypeName, name[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		if struct_regex.MatchString(code) {
			name := struct_regex.FindStringSubmatch(code)
			col += len(name[1])
			code = code[len(name[1]):]
			tok := Token{TokenTypeStruct, name[1], loc}
			tokens = append(tokens, tok)
			continue
		}
		fmt.Printf(
			"%s:%d:%d: SYNTAX ERROR: Unknown character: '%c'\n",
			append(loc.Get(), code[0])...,
		)
		os.Exit(1)
	}
	return tokens
}

var (
	macro_env      = map[string]interface{}{}
	symbol_env     = map[string]interface{}{}
	type_alias_env = map[string]interface{}{}
)

func parse_atom(tokens []Token, current_macros []string, macro_id int, eval bool) (interface{}, []Token) {
	if tokens[0].Type == TokenTypeInt {
		return Op{OpTypePushInt, tokens[0]}, tokens[1:]
	}
	if tokens[0].Type == TokenTypeChar {
		return Op{OpTypePushChar, tokens[0]}, tokens[1:]
	}
	if tokens[0].Type == TokenTypeString {
		chars := []Token{}
		for _, elem := range tokens[0].Value {
			chars = append(chars, Token{TokenTypeChar, string(elem), tokens[0].Location})
		}
		chars = append(chars, Token{TokenTypeNil, "[]", tokens[0].Location})
		for range tokens[0].Value {
			chars = append(chars, Token{TokenTypeName, ":>", tokens[0].Location})
		}
		return parse(append(chars, tokens[1:]...), current_macros, macro_id, eval)
	}
	if tokens[0].Type == TokenTypeNil {
		if len(tokens) > 1 {
			elem, tokens := parse_atom(tokens[1:], current_macros, macro_id, eval)
			if elem_type, ok := elem.(Type); ok {
				return Type{RawTypeList, []interface{}{elem_type}}, tokens
			}
		}
		return Op{OpTypePushNil, tokens[0]}, tokens[1:]
	}
	if tokens[0].Type == TokenTypeSymbol {
		new_value := tokens[0].Value
		var builder strings.Builder
		for _, cur_macro := range current_macros {
			builder.WriteString(cur_macro)
			builder.WriteString(" ")
		}
		builder.WriteString(fmt.Sprintf(" %d ", macro_id))
		builder.WriteString(new_value)
		new_tok := Token{tokens[0].Type, builder.String(), tokens[0].Location}
		symbol_env[new_tok.Value] = true
		return Op{OpTypeSaveSymbol, new_tok}, tokens[1:]
	}
	if tokens[0].Type == TokenTypeName {
		if tokens[0].Value == "assert" {
			return Op{OpTypeAssert, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == ":>" {
			return Op{OpTypeCons, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "." {
			return Op{OpTypeProp, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "~" {
			return Op{OpTypeSplice, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "++" {
			return Op{OpTypeAppend, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "+" {
			return Op{OpTypeAdd, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "-" {
			return Op{OpTypeSub, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "*" {
			return Op{OpTypeMul, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "/" {
			return Op{OpTypeDiv, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "%" {
			return Op{OpTypeMod, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "<" {
			return Op{OpTypeLt, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == ">" {
			return Op{OpTypeGt, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "=" {
			return Op{OpTypeEq, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "&" {
			return Op{OpTypeAnd, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "|" {
			return Op{OpTypeOr, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "print" {
			return Op{OpTypePrint, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "show" {
			return Op{OpTypeShow, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "dup" {
			return Op{OpTypeDup, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "swap" {
			return Op{OpTypeSwap, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "drop" {
			return Op{OpTypeDrop, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "over" {
			return Op{OpTypeOver, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "iota" {
			return Op{OpTypeIota, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "#" {
			return Op{OpTypeLen, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "head" {
			return Op{OpTypeHead, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "tail" {
			return Op{OpTypeTail, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "type?" {
			return Op{OpTypePrintType, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "stack?" {
			return Op{OpTypePrintStack, tokens[0]}, tokens[1:]
		}
		if tokens[0].Value == "then" {
			then := tokens[0]
			body, tokens := parse(tokens[1:], current_macros, macro_id, eval)
			else_ifs := []interface{}{}
			for tokens[0].Value == "elif" {
				if len(tokens) == 1 {
					fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'elif' condition\n", tokens[0].Location.Get()...)
					os.Exit(1)
				}
				cond, new_tokens := parse(tokens[1:], current_macros, macro_id, eval)
				if new_tokens[0].Value != "do" {
					fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'elif' body\n", tokens[0].Location.Get()...)
					os.Exit(1)
				}
				body, new_tokens := parse(new_tokens[1:], current_macros, macro_id, eval)
				tokens = new_tokens
				else_ifs = append(else_ifs, []interface{}{cond, body})
			}
			if tokens[0].Value == "end" {
				return Tree{TreeTypeThen, []interface{}{body, else_ifs}, then}, tokens[1:]
			}
			if tokens[0].Value == "else" {
				else_ := tokens[0]
				else_body, tokens := parse(tokens[1:], current_macros, macro_id, eval)
				if tokens[0].Value != "end" {
					fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'else' body\n", else_.Location.Get()...)
					os.Exit(1)
				}

				return Tree{TreeTypeThenElse, []interface{}{body, else_ifs, else_body}, then}, tokens[1:]
			}
			fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'then' block\n", then.Location.Get()...)
			os.Exit(1)
		}
		if tokens[0].Value == "while" {
			while := tokens[0]
			condition, tokens := parse(tokens[1:], current_macros, macro_id, eval)
			if len(tokens) == 0 {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'while' condition\n", while.Location.Get()...)
				os.Exit(1)
			}
			if tokens[0].Value != "loop" {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'while' condition\n", while.Location.Get()...)
				os.Exit(1)
			}
			loop := tokens[0]
			body, tokens := parse(tokens[1:], current_macros, macro_id, eval)
			if len(tokens) == 0 {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'loop' block\n", loop.Location.Get()...)
				os.Exit(1)
			}
			if tokens[0].Value != "end" {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated 'loop' block\n", loop.Location.Get()...)
			}
			return Tree{TreeTypeWhile, []interface{}{condition, body}, while}, tokens[1:]
		}
		if tokens[0].Value == "define" {
			macro := tokens[0]
			if len(tokens) <= 1 {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated macro (define) declaration\n", macro.Location.Get()...)
				os.Exit(1)
			}
			name := tokens[1]
			tokens = tokens[2:]
			_, new_tokens := parse(tokens, append(current_macros, name.Value), macro_id, false)
			body := tokens[:len(tokens)-len(new_tokens)]
			tokens = new_tokens
			if len(tokens) < 1 {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated macro (define) declaration\n", macro.Location.Get()...)
				os.Exit(1)
			}
			if tokens[0].Value == "end" {
				macro_env[name.Value] = body
				return Op{OpTypeNoOp, macro}, tokens[1:]
			}
			fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated macro (define) declaration\n", macro.Location.Get()...)
			os.Exit(1)
		}
		if tokens[0].Value == "include" {
			include := tokens[0]
			if len(tokens) == 1 {
				fmt.Printf("%s:%d:%d PARSE ERROR: Unterminated include statement\n", include.Location.Get()...)
				os.Exit(1)
			}
			path := tokens[1].Value
			path = strings.ReplaceAll(path, ".", "/") + ".stk"
			file, err := os.Open(path)
			if err != nil {
				fmt.Println("Error including file:", err)
			}
			defer file.Close()
			scanner := bufio.NewScanner(file)
			var builder strings.Builder
			for scanner.Scan() {
				builder.WriteString(scanner.Text())
				builder.WriteString("\n")
			}
			contents := builder.String()
			new_tokens := lex(path, contents)
			tree := parse_tokens(new_tokens)
			return tree, tokens[2:]
		}
		if tokens[0].Value == "type" {
			type_tok := tokens[0]
			if len(tokens) == 1 {
				fmt.Printf("%s:%d:%d PARSE ERROR: Unterminated type alias\n", type_tok.Location.Get()...)
				os.Exit(1)
			}
			name := tokens[1]
			body, tokens := parse_atom(tokens[2:], current_macros, macro_id, eval)
			if tokens[0].Value != "end" {
				fmt.Printf("%s:%d:%d PARSE ERROR: Unterminated type alias\n", type_tok.Location.Get()...)
				os.Exit(1)
			}
			type_alias_env[name.Value] = body
			return parse(tokens[1:], current_macros, macro_id, eval)
		}
		if tokens[0].Value == "struct" {
			struct_keyword := tokens[0]
			if len(tokens) == 1 {
				fmt.Printf("%s:%d:%d: PARSE ERROR: Expected struct name and definition\n", struct_keyword.Location.Get()...)
				os.Exit(1)
			}
			struct_name := tokens[1]
			fields := map[string]Type{}
			fields_order := []string{}
			tokens = tokens[2:]
			for {
				field_name := tokens[0]
				if field_name.Type == TokenTypeName && field_name.Value == "end" {
					break
				}
				if field_name.Type != TokenTypeString {
					fmt.Printf("%s:%d:%d: PARSE ERROR: Expected struct field name to be a string\n", struct_keyword.Location.Get()...)
					os.Exit(1)
				}
				field_type, new_tokens := parse_atom(tokens[1:], current_macros, macro_id, eval)
				tokens = new_tokens
				if field_type == nil {
					fmt.Printf("%s:%d:%d: PARSE ERROR: Expected type after struct field name\n", field_name.Location.Get()...)
					os.Exit(1)
				}
				if as_type, ok := field_type.(Type); ok {
					fields[field_name.Value] = as_type
					fields_order = append(fields_order, field_name.Value)
				}
				if as_symbol, ok := field_type.(Op); ok {
					fields[field_name.Value] = Type{RawTypeSymbolType, []interface{}{as_symbol}}
					fields_order = append(fields_order, field_name.Value)
				}
			}
			return Tree{TreeTypeStruct, []interface{}{struct_name, fields, fields_order}, struct_keyword}, tokens[1:]
		}
		macro_value, in_macros := macro_env[tokens[0].Value]
		if in_macros {
			in_current_macros := false
			for _, elem := range current_macros {
				if elem == tokens[0].Value {
					in_current_macros = true
				}
			}
			macro := tokens[0]
			if !in_current_macros {
				if macro_tokens, ok := macro_value.([]Token); ok {
					value, _ := parse(macro_tokens, append(current_macros, tokens[0].Value), macro_id+1, eval)
					value_expr := value.(Tree)
					rest_values, tokens := parse(tokens[1:], current_macros, macro_id, eval)
					rest_expr := rest_values.(Tree)
					return Tree{TreeTypeExpression, append(append(value_expr.Nodes, Op{OpTypeClearMacroSymbols, macro}), rest_expr.Nodes...), macro}, tokens
				}
			}
			fmt.Printf("%s:%d:%d: MACRO ERROR: Recursive macro declaration detected\n", tokens[0].Location.Get()...)
			os.Exit(1)
		}
		new_value := tokens[0].Value
		var builder strings.Builder
		for _, cur_macro := range current_macros {
			builder.WriteString(cur_macro)
			builder.WriteString(" ")
		}
		builder.WriteString(fmt.Sprintf(" %d ", macro_id))
		builder.WriteString(new_value)
		new_tok := Token{tokens[0].Type, builder.String(), tokens[0].Location}
		_, in_symbols := symbol_env[new_tok.Value]
		if in_symbols {
			return Op{OpTypeName, new_tok}, tokens[1:]
		}
		is_keyword := false
		for _, keyword := range []string{"then", "define", "else", "elif", "do", "end", "while", "loop", "struct"} {
			if tokens[0].Value == keyword {
				is_keyword = true
			}
		}
		if is_keyword {
			return nil, tokens
		}
		if eval {
			fmt.Printf("%s:%d:%d: PARSE ERROR: Unrecognized name: %s\n", append(tokens[0].Location.Get(), tokens[0].Value)...)
			os.Exit(1)
		}
		return Token{}, tokens[1:]
	}
	if tokens[0].Type == TokenTypeQuote {
		if tokens[0].Value == "{" {
			open_quote := tokens[0]
			body, tokens := parse(tokens[1:], current_macros, macro_id, false)
			if tokens[0].Value != "}" {
				fmt.Printf("%s:%d:%d PARSE ERROR: Unterminated quote definition\n", open_quote.Location.Get()...)
				os.Exit(1)
			}
			return Tree{TreeTypeQuote, []interface{}{body}, open_quote}, tokens[1:]
		}
	}
	if tokens[0].Type == TokenTypeStruct {
		struct_name := tokens[0]
		if len(tokens) == 1 {
			fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated struct definition\n", struct_name.Location.Get()...)
			os.Exit(1)
		}
		if tokens[1].Value != "{" {
			if type_value, in_env := type_alias_env[struct_name.Value]; in_env {
				return type_value.(Type), tokens[1:]
			}
			if struct_name.Value == "Int" {
				return Type{RawTypeInt, []interface{}{}}, tokens[1:]
			} else if struct_name.Value == "Char" {
				return Type{RawTypeChar, []interface{}{}}, tokens[1:]
			} else if struct_name.Value == "String" {
				return Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, tokens[1:]
			} else if struct_name.Value == "Undefined" {
				return Type{RawTypeUndefined, []interface{}{}}, tokens[1:]
			}
			return Type{RawTypeStruct, []interface{}{}}, tokens[1:]
		}
		body, tokens := parse(tokens[2:], current_macros, macro_id, eval)
		if tokens[0].Value != "}" {
			fmt.Printf("%s:%d:%d: PARSE ERROR: Unterminated struct definition\n", struct_name.Location.Get()...)
			os.Exit(1)
		}
		return Tree{TreeTypeNewStruct, []interface{}{body}, struct_name}, tokens[1:]
	}
	return nil, tokens
}

func parse(tokens []Token, current_macros []string, macro_id int, eval bool) (interface{}, []Token) {
	if len(tokens) == 0 {
		return Tree{TreeTypeExpression, []interface{}{}, Token{}}, tokens
	}
	first := tokens[0]
	ops := []interface{}{}
	for len(tokens) > 0 {
		op, new_tokens := parse_atom(tokens, current_macros, macro_id, eval)
		tokens = new_tokens
		if op == nil {
			break
		}
		ops = append(ops, op)
	}
	return Tree{TreeTypeExpression, ops, first}, tokens
}

func parse_tokens(tokens []Token) Tree {
	tree, tokens := parse(tokens, []string{}, 0, true)
	if len(tokens) > 0 {
		fmt.Printf("%s:%d:%d: PARSE ERROR: Invalid syntax\n", tokens[0].Location.Get()...)
		os.Exit(1)
	}
	if as_tree, ok := tree.(Tree); ok {
		return as_tree
	}
	return Tree{}
}

var op_type_names = map[int]string{}
var op_type_count int = 0
var type_names = map[int]string{}

func increase_op_type_count() int {
	value := op_type_count
	op_type_count += 1
	return value
}

func init_repr_maps() {
	op_type_names[increase_op_type_count()] = "noop"
	op_type_names[increase_op_type_count()] = "include"
	op_type_names[increase_op_type_count()] = ":>"
	op_type_names[increase_op_type_count()] = "assert"
	op_type_names[increase_op_type_count()] = "push int"
	op_type_names[increase_op_type_count()] = "push char"
	op_type_names[increase_op_type_count()] = "push nil"
	op_type_names[increase_op_type_count()] = "push quote"
	op_type_names[increase_op_type_count()] = "~"
	op_type_names[increase_op_type_count()] = "save symbol"
	op_type_names[increase_op_type_count()] = "struct"
	op_type_names[increase_op_type_count()] = "new struct"
	op_type_names[increase_op_type_count()] = "."
	op_type_names[increase_op_type_count()] = "+"
	op_type_names[increase_op_type_count()] = "-"
	op_type_names[increase_op_type_count()] = "*"
	op_type_names[increase_op_type_count()] = "/"
	op_type_names[increase_op_type_count()] = "%"
	op_type_names[increase_op_type_count()] = "<"
	op_type_names[increase_op_type_count()] = ">"
	op_type_names[increase_op_type_count()] = "="
	op_type_names[increase_op_type_count()] = "&"
	op_type_names[increase_op_type_count()] = "|"
	op_type_names[increase_op_type_count()] = "!"
	op_type_names[increase_op_type_count()] = "print"
	op_type_names[increase_op_type_count()] = "show"
	op_type_names[increase_op_type_count()] = "dup"
	op_type_names[increase_op_type_count()] = "swap"
	op_type_names[increase_op_type_count()] = "drop"
	op_type_names[increase_op_type_count()] = "over"
	op_type_names[increase_op_type_count()] = "iota"
	op_type_names[increase_op_type_count()] = "#"
	op_type_names[increase_op_type_count()] = "head"
	op_type_names[increase_op_type_count()] = "tail"
	op_type_names[increase_op_type_count()] = "++"
	op_type_names[increase_op_type_count()] = "name"
	op_type_names[increase_op_type_count()] = "'type?'"

	type_names[0] = "Int"
	type_names[1] = "Char"
	type_names[2] = "List"
	type_names[6] = "Undefined"
}

func get_type_repr(typ Type) string {
	name := type_names[typ.Type]
	if typ.Type == RawTypeStruct {
		name = typ.Args[0].(string)
	}
	var builder strings.Builder
	num_args := 0
	builder.WriteString(name)
	builder.WriteString("(")
	for i, elem := range typ.Args {
		if as_type, ok := elem.(Type); ok {
			if i > 0 {
				builder.WriteString(", ")
			}
			builder.WriteString(get_type_repr(as_type))
			num_args += 1
		}
	}
	if num_args == 0 {
		return name
	}
	builder.WriteString(")")
	return builder.String()
}

func not_enough_args(token Token, op_type OpType, num_args int, got int) {
	fmt.Printf("%s:%d:%d: TYPE ERROR: Not enough arguments for operand %s, expected at least %d arguments, got %d\n", append(token.Location.Get(), op_type_names[op_type], num_args, got)...)
	os.Exit(1)
}

func expected_type(token Token, typ Type, got Type, op_type OpType, pos string) {
	fmt.Printf("%s:%d:%d: TYPE ERROR: Invalid type for the %s argument of the %s operator, expected %s, got %s\n", append(token.Location.Get(), pos, op_type_names[op_type], get_type_repr(typ), get_type_repr(got))...)
	os.Exit(1)
}

func modifies_structure(token Token, structure string) {
	fmt.Printf("%s:%d:%d: TYPE ERROR: %s modifies the structure of the stack\n", append(token.Location.Get(), structure)...)
	os.Exit(1)
}

func cmp_types(a Type, b Type, tok Token) bool {
	if a.Type == RawTypeUndefined {
		return true
	}
	if b.Type == RawTypeUndefined {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	min := 0
	if len(a.Args) > len(b.Args) {
		min = len(b.Args)
	} else {
		min = len(a.Args)
	}
	if a.Type == RawTypeChar {
		return true
	}
	for i := range min {
		x := a.Args[i]
		y := b.Args[i]
		if x_as_type, ok := x.(Type); ok {
			if y_as_type, ok := y.(Type); ok {
				if !cmp_types(x_as_type, y_as_type, tok) {
					return false
				}
			}
		}
		if a.Type == RawTypeStruct {
			if x_as_string, ok := x.(string); ok {
				if y_as_string, ok := y.(string); ok {
					if x_as_string != y_as_string {
						return false
					}
				}
			}
			if x_as_list, ok := x.([]interface{}); ok {
				if y_as_list, ok := y.([]interface{}); ok {
					min := 0
					if len(x_as_list) > len(y_as_list) {
						min = len(y_as_list)
					} else {
						min = len(x_as_list)
					}
					for i := range min {
						if !cmp_types(x_as_list[i].(Type), y_as_list[i].(Type), tok) {
							return false
						}
					}
				}
			}
		}
	}
	return true
}

func unify_types(a Type, b Type) Type {
	if a.Type == RawTypeUndefined {
		return b
	}
	if b.Type == RawTypeUndefined {
		return a
	}
	if a.Type != b.Type {
		os.Exit(1)
	}
	if a.Type == RawTypeList {
		new_list := []interface{}{}
		for i, elem := range a.Args {
			if a_elem, ok := elem.(Type); ok {
				if b_elem, ok := b.Args[i].(Type); ok {
					new_list = append(new_list, unify_types(a_elem, b_elem))
				}
			}
		}
		return Type{RawTypeList, new_list}
	}
	return a
}

func instantiate_types(a Type, b Type) Type {
	if a.Type == RawTypeUndefined {
		return b
	}
	if b.Type == RawTypeUndefined {
		return a
	}
	if a.Type != b.Type {
		os.Exit(1)
	}
	if a.Type == RawTypeList {
		new_list := []interface{}{}
		for i, elem := range a.Args {
			if a_elem, ok := elem.(Type); ok {
				if b_elem, ok := b.Args[i].(Type); ok {
					new_list = append(new_list, instantiate_types(a_elem, b_elem))
				}
			}
		}
		return Type{RawTypeList, new_list}
	}
	return a
}

var (
	symbol_type_env   = map[string]Type{}
	struct_type_env   = map[string](map[string]Type){}
	struct_type_order = map[string][]string{}
)

func typecheck(program interface{}, stack []TypeStackEntry) []TypeStackEntry {
	if op, ok := program.(Op); ok {
		if op.Type == OpTypeNoOp {
			return stack
		}
		if op.Type == OpTypePushInt {
			return append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
		}
		if op.Type == OpTypePushChar {
			return append(stack, TypeStackEntry{Type{RawTypeChar, []interface{}{op.Value.Value}}, op.Value})
		}
		if op.Type == OpTypePushNil {
			stack = append(stack, TypeStackEntry{Type{RawTypeList, []interface{}{Type{RawTypeUndefined, []interface{}{}}}}, op.Value})
			return stack
		}
		if op.Type == OpTypeName {
			symbol_value, in_symbols := symbol_type_env[op.Value.Value]
			if in_symbols {
				stack = append(stack, TypeStackEntry{symbol_value, op.Value})
				return stack
			}
			fmt.Printf("%s:%d:%d: TYPE ERROR: Unknown symbol: %s\n", append(op.Value.Location.Get(), op.Value)...)
			os.Exit(1)
		}
		if op.Type == OpTypeAppend {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_b.Type, Type{RawTypeList, []interface{}{Type{RawTypeUndefined, []interface{}{}}}}, op.Value) {
				expected_type(entry_b.Token, Type{RawTypeList, []interface{}{}}, entry_b.Type, op.Type, "first")
			}
			if !cmp_types(entry_a.Type, entry_b.Type, op.Value) {
				expected_type(entry_a.Token, entry_b.Type, entry_a.Type, op.Type, "second")
			}
			list_type := unify_types(entry_a.Type, entry_b.Type)
			stack = append(stack, TypeStackEntry{list_type, op.Value})
			return stack
		}
		if op.Type == OpTypeSplice {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			quote_entry := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			// TODO: revisit quotes
			if quote_entry.Type.Type != RawTypeQuote {
				expected_type(quote_entry.Token, Type{RawTypeQuote, []interface{}{}}, quote_entry.Type, op.Type, "first")
			}
			body := quote_entry.Type.Args[0]
			stack := typecheck(body, stack)
			return stack
		}
		is_operator := false
		for _, operator := range []OpType{OpTypeAdd, OpTypeSub, OpTypeMul, OpTypeDiv, OpTypeMod, OpTypeLt, OpTypeGt} {
			if op.Type == operator {
				is_operator = true
			}
		}
		if is_operator {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_a.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeInt, []interface{}{}}, entry_a.Type, op.Type, "second")
			}
			if !cmp_types(entry_b.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_b.Token, Type{RawTypeInt, []interface{}{}}, entry_b.Type, op.Type, "first")
			}
			stack = append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
			return stack
		}
		if op.Type == OpTypeEq {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_a.Type, entry_b.Type, op.Value) {
				expected_type(op.Value, entry_b.Type, entry_a.Type, op.Type, "second")
			}
			stack = append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
			return stack
		}
		if op.Type == OpTypeAnd {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_a.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeInt, []interface{}{}}, entry_a.Type, op.Type, "second")
			}
			if !cmp_types(entry_b.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_b.Token, Type{RawTypeInt, []interface{}{}}, entry_b.Type, op.Type, "first")
			}
			stack = append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
			return stack
		}
		if op.Type == OpTypeOr {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_a.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeInt, []interface{}{}}, entry_a.Type, op.Type, "second")
			}
			if !cmp_types(entry_b.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_b.Token, Type{RawTypeInt, []interface{}{}}, entry_b.Type, op.Type, "first")
			}
			stack = append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
			return stack
		}
		if op.Type == OpTypePrint {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeShow {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			stack[len(stack)-1] = TypeStackEntry{Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, op.Value}
			return stack
		}
		if op.Type == OpTypeDup {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			stack = append(stack, stack[len(stack)-1])
			return stack
		}
		if op.Type == OpTypeSwap {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack[len(stack)-2] = entry_a
			stack[len(stack)-1] = entry_b
			return stack
		}
		if op.Type == OpTypeDrop {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeOver {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_b := stack[len(stack)-2]
			stack[len(stack)] = entry_b
			return stack
		}
		if op.Type == OpTypeIota {
			stack = append(stack, TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value})
			return stack
		}
		if op.Type == OpTypeLen {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			entry_a := stack[len(stack)-1]
			if !cmp_types(entry_a.Type, Type{RawTypeList, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeList, []interface{}{}}, entry_a.Type, OpTypeLen, "first")
			}
			stack[len(stack)-1] = TypeStackEntry{Type{RawTypeInt, []interface{}{}}, op.Value}
			return stack
		}
		if op.Type == OpTypeHead {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			entry_a := stack[len(stack)-1]
			if !cmp_types(entry_a.Type, Type{RawTypeList, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeList, []interface{}{}}, entry_a.Type, OpTypeHead, "first")
			}
			if len(entry_a.Type.Args) == 0 {
				stack[len(stack)-1] = TypeStackEntry{Type{RawTypeUndefined, []interface{}{}}, op.Value}
			} else if arg_as_type, ok := entry_a.Type.Args[0].(Type); ok {
				stack[len(stack)-1] = TypeStackEntry{arg_as_type, op.Value}
			}
			return stack
		}
		if op.Type == OpTypeTail {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			entry_a := stack[len(stack)-1]
			if !cmp_types(entry_a.Type, Type{RawTypeList, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeList, []interface{}{}}, entry_a.Type, OpTypeTail, "first")
			}
			stack[len(stack)-1] = TypeStackEntry{Type{RawTypeList, entry_a.Type.Args}, op.Value}
			return stack
		}
		if op.Type == OpTypePrintType {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			entry_a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			fmt.Printf("%s:%d:%d (type?): %s\n", append(op.Value.Location.Get(), get_type_repr(entry_a.Type))...)
			return stack
		}
		if op.Type == OpTypePrintStack {
			var builder strings.Builder
			builder.WriteString("(stack?): [")
			for i := len(stack) - 1; i >= 0; i-- {
				builder.WriteString("\n  ")
				builder.WriteString(fmt.Sprintf("%d := ", len(stack)-i-1))
				builder.WriteString(get_type_repr(stack[i].Type))
			}
			builder.WriteString("\n]\n")
			fmt.Printf("%s:%d:%d %s", append(op.Value.Location.Get(), builder.String())...)
			return stack
		}
		if op.Type == OpTypeClearMacroSymbols {
			for name := range symbol_type_env {
				if strings.Contains(name, op.Value.Value+"  ") {
					delete(symbol_type_env, name)
				}
			}
			return stack
		}
		if op.Type == OpTypeSaveSymbol {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			if value, ok := symbol_type_env[op.Value.Value]; ok {
				if !cmp_types(stack[len(stack)-1].Type, value, op.Value) {
					parts := strings.Split(op.Value.Value, " ")
					if get_type_repr(value) == get_type_repr(stack[len(stack)-1].Type) {
						fmt.Printf("%s:%d:%d: TYPE ERROR: Attempting to re-assign value to symbol ':%s' with a different type, symbol has type %s, while the value has type %s (different instantiation)\n", append(op.Value.Location.Get(), parts[len(parts)-1], get_type_repr(value), get_type_repr(stack[len(stack)-1].Type))...)
						os.Exit(1)
					}
					fmt.Printf("%s:%d:%d: TYPE ERROR: Attempting to re-assign value to symbol ':%s' with a different type, symbol has type %s, while the value has type %s\n", append(op.Value.Location.Get(), parts[len(parts)-1], get_type_repr(value), get_type_repr(stack[len(stack)-1].Type))...)
					os.Exit(1)
				}
			}
			symbol_type_env[op.Value.Value] = stack[len(stack)-1].Type
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeAssert {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_a.Type, Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, entry_a.Type, op.Type, "second")
			}
			if !cmp_types(entry_b.Type, Type{RawTypeInt, []interface{}{}}, op.Value) {
				expected_type(entry_b.Token, Type{RawTypeInt, []interface{}{}}, entry_b.Type, op.Type, "first")
			}
			return stack
		}
		if op.Type == OpTypeCons {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			if !cmp_types(entry_a.Type, Type{RawTypeList, []interface{}{}}, op.Value) {
				expected_type(entry_a.Token, Type{RawTypeList, []interface{}{}}, entry_a.Type, OpTypeCons, "second (tail)")
			}
			if len(entry_a.Type.Args) > 0 {
				if !cmp_types(entry_a.Type.Args[0].(Type), entry_b.Type, op.Value) {
					expected_type(entry_b.Token, entry_a.Type.Args[0].(Type), entry_b.Type, OpTypeCons, "first (head)")
				}
			}
			stack = stack[:len(stack)-2]
			if entry_b.Type.Type == RawTypeChar {
				if len(entry_a.Type.Args) == 2 {
					stack = append(stack, TypeStackEntry{Type{entry_a.Type.Type, []interface{}{entry_b.Type, append([]string{entry_b.Type.Args[0].(string)}, entry_a.Type.Args[1].([]string)...)}}, op.Value})
				} else {
					stack = append(stack, TypeStackEntry{Type{entry_a.Type.Type, []interface{}{entry_b.Type, []string{entry_b.Type.Args[0].(string)}}}, op.Value})
				}
			} else {
				stack = append(stack, TypeStackEntry{Type{entry_a.Type.Type, []interface{}{entry_b.Type}}, op.Value})
			}
			return stack
		}
		if op.Type == OpTypeProp {
			if len(stack) < 2 {
				not_enough_args(op.Value, op.Type, 2, len(stack))
			}
			entry_prop := stack[len(stack)-1]
			entry_struct := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if !cmp_types(entry_prop.Type, Type{RawTypeList, []interface{}{}}, op.Value) {
				expected_type(entry_prop.Token, Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, entry_prop.Type, op.Type, "second")
			}
			if prop_elem_type, ok := entry_prop.Type.Args[0].(Type); ok {
				if !cmp_types(prop_elem_type, Type{RawTypeChar, []interface{}{}}, op.Value) {
					expected_type(entry_prop.Token, Type{RawTypeList, []interface{}{Type{RawTypeChar, []interface{}{}}}}, entry_prop.Type, op.Type, "second")
				}
				struct_name := entry_struct.Type.Args[0].(string)
				struct_fields, in_env := struct_type_env[struct_name]
				if !in_env {
					fmt.Printf("%s:%d:%d: TYPE ERROR: Unknown struct: %s\n", append(entry_struct.Token.Location.Get(), struct_name)...)
					os.Exit(1)
				}
				prop_name := entry_prop.Type.Args[1].([]string)
				str_prop := []byte{}
				for _, char := range prop_name {
					str_prop = append(str_prop, char[0])
				}
				field_value, in_fields := struct_fields[string(str_prop)]
				if !in_fields {
					fmt.Printf("%s:%d:%d: TYPE ERROR: Unknown struct field for struct %s: %s\n", append(op.Value.Location.Get(), struct_name, string(str_prop))...)
					os.Exit(1)
				}
				return append(stack, TypeStackEntry{field_value, op.Value})
			}
			fmt.Println("Unreachable")
			os.Exit(1)
		}
		fmt.Println("Not implemented:", op_type_names[op.Type])
		os.Exit(1)
	}
	if tree, ok := program.(Tree); ok {
		if tree.Type == TreeTypeStruct {
			name := tree.Nodes[0]
			fields := tree.Nodes[1]
			fields_order := tree.Nodes[2]
			if str_name, ok := name.(Token); ok {
				if map_fields, ok := fields.(map[string]Type); ok {
					new_map_fields := map[string]Type{}
					for key, typ := range map_fields {
						if typ.Type == RawTypeSymbolType {
							new_map_fields[key] = symbol_type_env[typ.Args[0].(Op).Value.Value]
						} else {
							new_map_fields[key] = typ
						}
					}
					if arr_order, ok := fields_order.([]string); ok {
						struct_type_env[str_name.Value] = new_map_fields
						struct_type_order[str_name.Value] = arr_order
					}
				}
			}
			return stack
		}
		if tree.Type == TreeTypeNewStruct {
			name := tree.Token.Value
			fields_arr := tree.Nodes[0]
			field_stack := typecheck(fields_arr, []TypeStackEntry{})
			struct_fields, in_env := struct_type_env[name]
			if name == "List" {
				new_fields := []interface{}{Op{OpTypePushNil, tree.Token}}
				for i := 0; i < len(field_stack); i++ {
					new_fields = append(new_fields, Op{OpTypeCons, tree.Token})
				}
				stack = typecheck(Tree{TreeTypeExpression, new_fields, tree.Token}, append(stack, field_stack...))
				return stack
			}
			if !in_env {
				fmt.Printf("%s:%d:%d: TYPE ERROR: Unknown struct name: %s\n", append(tree.Token.Location.Get(), name)...)
				os.Exit(1)
			}
			if len(struct_fields) != len(field_stack) {
				fmt.Printf("%s:%d:%d: TYPE ERROR: Invalid number of arguments for struct %s, expected %d, got %d\n", append(tree.Token.Location.Get(), name, len(struct_fields), len(field_stack))...)
				os.Exit(1)
			}
			fields_order := struct_type_order[name]
			new_fields := []interface{}{}
			for i, field_name := range fields_order {
				field := struct_fields[field_name]
				if !cmp_types(field_stack[i].Type, field, tree.Token) {
					fmt.Printf("%s:%d:%d: TYPE ERROR: Type mismatch between field %s and provided value of struct %s, expected %s, got %s\n", append(tree.Token.Location.Get(), field_name, name, get_type_repr(field), get_type_repr(field_stack[i].Type))...)
					os.Exit(1)
				}
				new_fields = append(new_fields, field_stack[i].Type)
			}
			return append(stack, TypeStackEntry{Type{RawTypeStruct, []interface{}{name, new_fields}}, tree.Token})
		}
		if tree.Type == TreeTypeQuote {
			return append(stack, TypeStackEntry{Type{RawTypeQuote, []interface{}{tree.Nodes[0]}}, tree.Token})
		}
		if tree.Type == TreeTypeExpression {
			for _, tr := range tree.Nodes {
				stack = typecheck(tr, stack)
			}
			return stack
		}
		if tree.Type == TreeTypeThen {
			if len(stack) == 0 {
				not_enough_args(tree.Token, tree.Type, 1, 0)
			}
			cond := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !cmp_types(cond.Type, Type{RawTypeInt, []interface{}{}}, tree.Token) {
				expected_type(cond.Token, Type{RawTypeInt, []interface{}{}}, cond.Type, TreeTypeThen, "main")
			}
			new := stack
			new = typecheck(tree.Nodes[0], new)
			if len(stack) != len(new) {
				modifies_structure(tree.Token, "'then' body")
			}
			for i, a := range stack {
				b := new[i]
				if !cmp_types(a.Type, b.Type, tree.Token) {
					modifies_structure(b.Token, "'then' body")
				}
			}
			return stack
		}
		if tree.Type == TreeTypeThenElse {
			cond := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			copy_stack_1 := make([]TypeStackEntry, len(stack))
			copy_stack_2 := make([]TypeStackEntry, len(stack))
			copy(copy_stack_1, stack)
			copy(copy_stack_2, stack)
			if !cmp_types(cond.Type, Type{RawTypeInt, []interface{}{}}, tree.Token) {
				expected_type(cond.Token, Type{RawTypeInt, []interface{}{}}, cond.Type, TreeTypeThen, "first")
			}
			then_stack := typecheck(tree.Nodes[0], copy_stack_1)
			for _, elseif_branch := range tree.Nodes[0].(Tree).Nodes {
				copy_stack_3 := make([]TypeStackEntry, len(stack))
				copy(copy_stack_3, stack)
				elseif_stack := typecheck(elseif_branch, copy_stack_3)
				if len(then_stack) != len(elseif_stack) {
					modifies_structure(tree.Token, "'elif' body")
				}
				for i := range len(then_stack) {
					a := then_stack[i]
					b := elseif_stack[i]
					if !cmp_types(a.Type, b.Type, tree.Token) {
						modifies_structure(b.Token, "'elif' body")
					}
					then_stack[i] = TypeStackEntry{instantiate_types(a.Type, b.Type), a.Token}
				}
			}
			else_stack := typecheck(tree.Nodes[2], copy_stack_2)
			if len(then_stack) != len(else_stack) {
				modifies_structure(tree.Token, "'else' body")
			}
			for i := range len(then_stack) {
				a := then_stack[i]
				b := else_stack[i]
				if !cmp_types(a.Type, b.Type, tree.Token) {
					modifies_structure(b.Token, "'else' body")
				}
				then_stack[i] = TypeStackEntry{instantiate_types(a.Type, b.Type), a.Token}
			}
			return then_stack
		}
		if tree.Type == TreeTypeWhile {
			new := make([]TypeStackEntry, len(stack))
			copy(new, stack)
			new = typecheck(tree.Nodes[0], new)
			new = new[:len(new)-1]
			if len(stack) != len(new) {
				modifies_structure(tree.Token, "'while' condition")
			}
			for i, a := range stack {
				b := new[i]
				if !cmp_types(a.Type, b.Type, tree.Token) {
					modifies_structure(b.Token, "'while' condition")
				}
			}
			new = typecheck(tree.Nodes[1], new)
			if len(stack) != len(new) {
				modifies_structure(tree.Token, "'while' body")
			}
			for i, a := range stack {
				b := new[i]
				if !cmp_types(a.Type, b.Type, tree.Token) {
					modifies_structure(b.Token, "'while' body")
				}
			}
			return stack
		}
		fmt.Println("Not implemented:", tree.Type)
		os.Exit(1)
	}
	return stack
}

func value_repr(value interface{}) string {
	if struct_val, ok := value.(ValueStruct); ok {
		var builder strings.Builder
		builder.WriteString(struct_val.Name)
		builder.WriteString("{")
		for i, elem := range struct_val.Values {
			if i > 0 {
				builder.WriteString(" ")
			}
			builder.WriteString(value_repr(elem))
		}
		builder.WriteString("}")
		return builder.String()
	}
	if arr_val, ok := value.([]interface{}); ok {
		all_bytes := true
		for _, elem := range arr_val {
			if _, ok := elem.(byte); !ok {
				all_bytes = false
			}
		}
		if all_bytes {
			var builder strings.Builder
			builder.WriteByte('"')
			for _, b := range arr_val {
				builder.WriteByte(b.(byte))
			}
			builder.WriteByte('"')
			return builder.String()
		}
		var builder strings.Builder
		builder.WriteString("List{")
		for i, elem := range arr_val {
			if i > 0 {
				builder.WriteString(" ")
			}
			builder.WriteString(value_repr(elem))
		}
		builder.WriteString("}")
		return builder.String()
	}
	if byte_val, ok := value.(byte); ok {
		var builder strings.Builder
		builder.WriteByte('\'')
		builder.WriteByte(byte_val)
		builder.WriteByte('\'')
		return builder.String()
	}
	return fmt.Sprintf("%v", value)
}

func cmp_values(a interface{}, b interface{}) bool {
	switch a.(type) {
	case []interface{}:
		min := 0
		if len(a.([]interface{})) > len(b.([]interface{})) {
			min = len(b.([]interface{}))
		} else {
			min = len(a.([]interface{}))
		}
		if min != len(a.([]interface{})) || min != len(b.([]interface{})) {
			return false
		}
		for i := range min {
			a_elem := a.([]interface{})[i]
			b_elem := b.([]interface{})[i]
			if !cmp_values(a_elem, b_elem) {
				return false
			}
		}
	case ValueStruct:
		min := 0
		if len(a.(ValueStruct).Values) > len(b.(ValueStruct).Values) {
			min = len(b.(ValueStruct).Values)
		} else {
			min = len(a.(ValueStruct).Values)
		}
		for i := range min {
			a_elem := a.(ValueStruct).Values[i]
			b_elem := b.(ValueStruct).Values[i]
			if !cmp_values(a_elem, b_elem) {
				return false
			}
		}
	default:
		return a == b
	}
	return true
}

var (
	symbol_value_env = map[string]StackEntry{}
	iota_counter     = 0
)

func interpret(program interface{}, stack []StackEntry) []StackEntry {
	if op, ok := program.(Op); ok {
		if op.Type == OpTypeNoOp {
			return stack
		}
		if op.Type == OpTypePushInt {
			n, _ := strconv.Atoi(op.Value.Value)
			return append(stack, StackEntry{n, op.Value})
		}
		if op.Type == OpTypePushChar {
			return append(stack, StackEntry{byte(op.Value.Value[0]), op.Value})
		}
		if op.Type == OpTypePushNil {
			stack = append(stack, StackEntry{[]interface{}{}, op.Value})
			return stack
		}
		if op.Type == OpTypeName {
			symbol_value, in_symbols := symbol_value_env[op.Value.Value]
			if in_symbols {
				stack = append(stack, StackEntry{symbol_value.Value, op.Value})
				return stack
			}
			fmt.Printf("%s:%d:%d: TYPE ERROR: Unknown symbol: %s\n", append(op.Value.Location.Get(), op.Value)...)
			os.Exit(1)
		}
		if op.Type == OpTypeSplice {
			quote_entry := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			body := quote_entry.Value
			stack := interpret(body, stack)
			return stack
		}
		is_operator := false
		for _, operator := range []OpType{OpTypeAdd, OpTypeSub, OpTypeMul, OpTypeDiv, OpTypeMod, OpTypeLt, OpTypeGt} {
			if op.Type == operator {
				is_operator = true
			}
		}
		if is_operator {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if a_int, ok := entry_a.Value.(int); ok {
				if b_int, ok := entry_b.Value.(int); ok {
					if op.Type == OpTypeAdd {
						stack = append(stack, StackEntry{b_int + a_int, op.Value})
					} else if op.Type == OpTypeSub {
						stack = append(stack, StackEntry{b_int - a_int, op.Value})
					} else if op.Type == OpTypeMul {
						stack = append(stack, StackEntry{b_int * a_int, op.Value})
					} else if op.Type == OpTypeDiv {
						stack = append(stack, StackEntry{b_int / a_int, op.Value})
					} else if op.Type == OpTypeMod {
						stack = append(stack, StackEntry{b_int % a_int, op.Value})
					} else if op.Type == OpTypeLt {
						if b_int < a_int {
							stack = append(stack, StackEntry{1, op.Value})
						} else {
							stack = append(stack, StackEntry{0, op.Value})
						}
					} else if op.Type == OpTypeGt {
						if b_int > a_int {
							stack = append(stack, StackEntry{1, op.Value})
						} else {
							stack = append(stack, StackEntry{0, op.Value})
						}
					}
				}
			}
			return stack
		}
		if op.Type == OpTypeEq {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if cmp_values(entry_a.Value, entry_b.Value) {
				stack = append(stack, StackEntry{1, op.Value})
			} else {
				stack = append(stack, StackEntry{0, op.Value})
			}
			return stack
		}
		if op.Type == OpTypeAnd {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if entry_a.Value.(int) != 0 && entry_b.Value.(int) != 0 {
				stack = append(stack, StackEntry{1, op.Value})
			} else {
				stack = append(stack, StackEntry{0, op.Value})
			}
			return stack
		}
		if op.Type == OpTypeOr {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if entry_a.Value.(int) != 0 || entry_b.Value.(int) != 0 {
				stack = append(stack, StackEntry{1, op.Value})
			} else {
				stack = append(stack, StackEntry{0, op.Value})
			}
			return stack
		}
		if op.Type == OpTypePrint {
			entry_a := stack[len(stack)-1]
			repr := value_repr(entry_a.Value)
			if repr[0] == '"' && repr[len(repr)-1] == '"' {
				repr = repr[1 : len(repr)-1]
			}
			fmt.Println(repr)
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeShow {
			if len(stack) < 1 {
				not_enough_args(op.Value, op.Type, 1, 0)
			}
			entry_a := stack[len(stack)-1]
			repr := []interface{}{}
			for _, char := range value_repr(entry_a.Value) {
				repr = append(repr, byte(char))
			}
			stack[len(stack)-1] = StackEntry{repr, op.Value}
			return stack
		}
		if op.Type == OpTypeDup {
			stack = append(stack, stack[len(stack)-1])
			return stack
		}
		if op.Type == OpTypeSwap {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack[len(stack)-2] = entry_a
			stack[len(stack)-1] = entry_b
			return stack
		}
		if op.Type == OpTypeDrop {
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeOver {
			entry_b := stack[len(stack)-2]
			stack[len(stack)] = entry_b
			return stack
		}
		if op.Type == OpTypeIota {
			stack = append(stack, StackEntry{iota_counter, op.Value})
			iota_counter += 1
			return stack
		}
		if op.Type == OpTypeLen {
			entry_a := stack[len(stack)-1]
			if a_arr, ok := entry_a.Value.([]interface{}); ok {
				stack[len(stack)-1] = StackEntry{len(a_arr), op.Value}
			}
			return stack
		}
		if op.Type == OpTypeHead {
			entry_a := stack[len(stack)-1]
			if a_arr, ok := entry_a.Value.([]interface{}); ok {
				if len(a_arr) == 0 {
					fmt.Printf("%s:%d:%d: ERROR: Head of an empty list\n", op.Value.Location.Get()...)
					os.Exit(1)
				}
				stack[len(stack)-1] = StackEntry{a_arr[0], op.Value}
			}
			return stack
		}
		if op.Type == OpTypeTail {
			entry_a := stack[len(stack)-1]
			if a_arr, ok := entry_a.Value.([]interface{}); ok {
				stack[len(stack)-1] = StackEntry{a_arr[1:], op.Value}
			}
			return stack
		}
		if op.Type == OpTypePrintType {
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeAppend {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if a_arr, ok := entry_a.Value.([]interface{}); ok {
				if b_arr, ok := entry_b.Value.([]interface{}); ok {
					stack = append(stack, StackEntry{append(b_arr, a_arr...), op.Value})
				}
			}
			return stack
		}
		if op.Type == OpTypeSaveSymbol {
			symbol_value_env[op.Value.Value] = StackEntry{stack[len(stack)-1].Value, op.Value}
			return stack[:len(stack)-1]
		}
		if op.Type == OpTypeAssert {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if entry_b.Value.(int) == 0 {
				fmt.Printf("%s:%d:%d: ASSERTION ERROR: %s\n", append(op.Value.Location.Get(), value_repr(entry_a.Value))...)
				os.Exit(1)
			}
			return stack
		}
		if op.Type == OpTypeCons {
			entry_a := stack[len(stack)-1]
			entry_b := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			if a_arr, ok := entry_a.Value.([]interface{}); ok {
				stack = append(stack, StackEntry{append([]interface{}{entry_b.Value}, a_arr...), op.Value})
			}
			return stack
		}
		if op.Type == OpTypeProp {
			entry_prop := stack[len(stack)-1]
			entry_struct := stack[len(stack)-2]
			stack = stack[:len(stack)-2]
			struct_fields := entry_struct.Value.(ValueStruct).Values
			prop_name := entry_prop.Value.([]interface{})
			byte_array := []byte{}
			for _, char := range prop_name {
				byte_array = append(byte_array, char.(byte))
			}
			fields_order := struct_type_order[entry_struct.Value.(ValueStruct).Name]
			prop_index := -1
			for i, field := range fields_order {
				if field == string(byte_array) {
					prop_index = i
					break
				}
			}
			field_value := struct_fields[prop_index]
			return append(stack, StackEntry{field_value, op.Value})
		}
		if op.Type == OpTypeClearMacroSymbols {
			return stack
		}
	}
	if tree, ok := program.(Tree); ok {
		if tree.Type == TreeTypeStruct {
			return stack
		}
		if tree.Type == TreeTypeNewStruct {
			name := tree.Token.Value
			fields_arr := tree.Nodes[0]
			field_stack := interpret(fields_arr, []StackEntry{})
			fields_order := struct_type_order[name]
			if name == "List" {
				new_fields := []interface{}{Op{OpTypePushNil, tree.Token}}
				for i := 0; i < len(field_stack); i++ {
					new_fields = append(new_fields, Op{OpTypeCons, tree.Token})
				}
				stack = interpret(Tree{TreeTypeExpression, new_fields, tree.Token}, append(stack, field_stack...))
				return stack
			}
			new_fields := []interface{}{}
			for i := range fields_order {
				new_fields = append(new_fields, field_stack[i].Value)
			}
			return append(stack, StackEntry{ValueStruct{name, new_fields}, tree.Token})
		}
		if tree.Type == TreeTypeQuote {
			return append(stack, StackEntry{tree.Nodes[0], tree.Token})
		}
		if tree.Type == TreeTypeExpression {
			for _, tr := range tree.Nodes {
				stack = interpret(tr, stack)
			}
			return stack
		}
		if tree.Type == TreeTypeThen {
			cond := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			elifs := tree.Nodes[1].([]interface{})
			if cond_int, ok := cond.Value.(int); ok {
				if cond_int == 1 {
					stack = interpret(tree.Nodes[0], stack)
					return stack
				}
				for _, elif := range elifs {
					stack = interpret(elif.([]interface{})[0], stack)
					cond := stack[len(stack)-1]
					stack = stack[:len(stack)-1]
					if cond_int, ok := cond.Value.(int); ok {
						if cond_int != 0 {
							stack = interpret(elif.([]interface{})[1], stack)
							break
						}
					}
				}
			}
			return stack
		}
		if tree.Type == TreeTypeThenElse {
			cond := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			elifs := tree.Nodes[1].([]interface{})
			if cond_int, ok := cond.Value.(int); ok {
				if cond_int != 0 {
					stack = interpret(tree.Nodes[0], stack)
				} else {
					elif_is_true := false
					for _, elif := range elifs {
						stack = interpret(elif.([]interface{})[0], stack)
						cond := stack[len(stack)-1]
						stack = stack[:len(stack)-1]
						if cond_int, ok := cond.Value.(int); ok {
							if cond_int != 0 {
								stack = interpret(elif.([]interface{})[1], stack)
								elif_is_true = true
								break
							}
						}
					}
					if !elif_is_true {
						stack = interpret(tree.Nodes[2], stack)
					}
				}
			}
			return stack
		}
		if tree.Type == TreeTypeWhile {
			cond := tree.Nodes[0]
			for {
				stack = interpret(cond, stack)
				result := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				if cond_int, ok := result.Value.(int); ok {
					if cond_int == 1 {
						stack = interpret(tree.Nodes[1], stack)
					} else {
						break
					}
				}
			}
			return stack
		}
	}
	return stack
}

func main() {
	file, err := os.Open("main.stk")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()
	init_repr_maps()

	var builder strings.Builder

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		builder.WriteString(scanner.Text())
		builder.WriteString("\n")
	}

	text := builder.String()

	tokens := lex("main.stk", text)
	tree := parse_tokens(tokens)
	type_stack := typecheck(tree, []TypeStackEntry{})
	if len(type_stack) > 0 {
		fmt.Printf("%s:%d:%d: TYPE ERROR: Unhandled data on the stack\n", type_stack[len(type_stack)-1].Token.Location.Get()...)
		os.Exit(1)
	}
	interpret(tree, []StackEntry{})
}
