// Package core provides the clj-yaml.core API for Glojure.
package core

import (
	"bytes"
	"fmt"
	"io"

	"github.com/glojurelang/glojure/pkg/lang"
	"github.com/glojurelang/glojure/pkg/runtime"
	"gopkg.in/yaml.v3"
)

func init() {
	runtime.RegisterNSLoader("clj_yaml/core", LoadNS)
}

// LoadNS registers the clj-yaml.core compatibility namespace.
func LoadNS() {
	ns := lang.FindOrCreateNamespace(lang.NewSymbol("clj-yaml.core"))
	ns.InternWithValue(
		lang.NewSymbol("parse-string"),
		lang.NewFnFunc(parseString),
		true,
	)
	ns.InternWithValue(
		lang.NewSymbol("generate-string"),
		lang.NewFnFunc(generateString),
		true,
	)
}

func parseString(arguments ...any) any {
	if len(arguments) < 1 {
		panic(lang.NewIllegalArgumentError("wrong number of arguments (0)"))
	}

	decoder := yaml.NewDecoder(bytes.NewBufferString(lang.ToString(arguments[0])))
	var document yaml.Node
	if err := decoder.Decode(&document); err != nil {
		panic(fmt.Errorf("YAML parse error: %w", err))
	}
	if len(document.Content) == 0 {
		return nil
	}
	return yamlNodeToGlojure(document.Content[0])
}

func yamlNodeToGlojure(node *yaml.Node) any {
	switch node.Kind {
	case yaml.MappingNode:
		values := make([]any, 0, len(node.Content))
		for index := 0; index < len(node.Content); index += 2 {
			values = append(
				values,
				yamlNodeToGlojure(node.Content[index]),
				yamlNodeToGlojure(node.Content[index+1]),
			)
		}
		return lang.NewPersistentArrayMapAsIfByAssoc(values)

	case yaml.SequenceNode:
		values := make([]any, 0, len(node.Content))
		for _, child := range node.Content {
			values = append(values, yamlNodeToGlojure(child))
		}
		return lang.NewVector(values...)

	case yaml.AliasNode:
		return yamlNodeToGlojure(node.Alias)

	case yaml.ScalarNode:
		var value any
		if err := node.Decode(&value); err != nil {
			panic(fmt.Errorf("YAML scalar error: %w", err))
		}
		return normalizeScalar(value)

	default:
		return nil
	}
}

func normalizeScalar(value any) any {
	switch value := value.(type) {
	case int:
		return int64(value)
	case int8:
		return int64(value)
	case int16:
		return int64(value)
	case int32:
		return int64(value)
	case uint:
		return int64(value)
	case uint8:
		return int64(value)
	case uint16:
		return int64(value)
	case uint32:
		return int64(value)
	default:
		return value
	}
}

func generateString(arguments ...any) any {
	if len(arguments) < 1 {
		panic(lang.NewIllegalArgumentError("wrong number of arguments (0)"))
	}

	var output bytes.Buffer
	encoder := yaml.NewEncoder(&output)
	encoder.SetIndent(2)
	if err := encoder.Encode(glojureToYAMLNode(arguments[0])); err != nil {
		panic(fmt.Errorf("YAML generation error: %w", err))
	}
	if err := encoder.Close(); err != nil {
		panic(fmt.Errorf("YAML generation error: %w", err))
	}
	return output.String()
}

func glojureToYAMLNode(value any) *yaml.Node {
	if mapping, ok := value.(lang.IPersistentMap); ok {
		node := &yaml.Node{Kind: yaml.MappingNode, Tag: "!!map"}
		for entries := mapping.Seq(); entries != nil; entries = entries.Next() {
			entry, ok := entries.First().(lang.IMapEntry)
			if !ok {
				panic(fmt.Errorf("YAML generation error: invalid map entry"))
			}
			node.Content = append(
				node.Content,
				glojureToYAMLNode(entry.Key()),
				glojureToYAMLNode(entry.Val()),
			)
		}
		return node
	}

	if _, ok := value.(lang.Sequential); ok {
		node := &yaml.Node{Kind: yaml.SequenceNode, Tag: "!!seq"}
		for values := lang.Seq(value); values != nil; values = values.Next() {
			node.Content = append(node.Content, glojureToYAMLNode(values.First()))
		}
		return node
	}

	node := &yaml.Node{}
	if keyword, ok := value.(lang.Keyword); ok {
		value = keyword.String()
	}
	if symbol, ok := value.(*lang.Symbol); ok {
		value = symbol.String()
	}
	if err := node.Encode(value); err != nil {
		if err == io.EOF {
			return &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!null"}
		}
		panic(fmt.Errorf("YAML generation error: %w", err))
	}
	return node
}
