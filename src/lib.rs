use std::{
    collections::{HashMap, HashSet},
    iter::{empty, once},
};
use wasm_bindgen::prelude::*;

use anyhow::anyhow;
use text_utils_grammar::{LR1GrammarParser, LR1Parse};

static LEXER: &str = include_str!("../grammar/sparql.l");
static GRAMMAR: &str = include_str!("../grammar/sparql.y");

fn find_all<'a, 's>(
    parse: &'a LR1Parse<'s>,
    name: &'a str,
    skip: &'a HashSet<&str>,
) -> Box<dyn Iterator<Item = &'a LR1Parse<'s>> + 'a> {
    match parse {
        LR1Parse::Empty(n) | LR1Parse::Terminal(n, ..) | LR1Parse::NonTerminal(n, ..)
            if skip.contains(n) =>
        {
            Box::new(empty())
        }
        LR1Parse::Empty(n) | LR1Parse::Terminal(n, ..) | LR1Parse::NonTerminal(n, ..)
            if name == *n =>
        {
            Box::new(once(parse))
        }
        LR1Parse::NonTerminal(.., children) => Box::new(
            children
                .iter()
                .flat_map(|child| find_all(child, name, skip)),
        ),
        _ => Box::new(empty()),
    }
}

fn find_all_mut<'a, 'g>(
    parse: &'a mut LR1Parse<'g>,
    name: &'a str,
    skip: &'a HashSet<&str>,
) -> Box<dyn Iterator<Item = &'a mut LR1Parse<'g>> + 'a> {
    match parse {
        LR1Parse::Empty(n) | LR1Parse::Terminal(n, ..) | LR1Parse::NonTerminal(n, ..)
            if skip.contains(n) =>
        {
            Box::new(empty())
        }
        LR1Parse::Empty(n) | LR1Parse::Terminal(n, ..) | LR1Parse::NonTerminal(n, ..)
            if name == *n =>
        {
            Box::new(once(parse))
        }
        LR1Parse::NonTerminal(.., children) => Box::new(
            children
                .iter_mut()
                .flat_map(|child| find_all_mut(child, name, skip)),
        ),
        _ => Box::new(empty()),
    }
}

fn find_mut<'a, 'g>(
    parse: &'a mut LR1Parse<'g>,
    name: &'a str,
    skip: &'a HashSet<&str>,
) -> Option<&'a mut LR1Parse<'g>> {
    find_all_mut(parse, name, skip).next()
}

#[wasm_bindgen]
pub struct SPARQLFormatter {
    parser: LR1GrammarParser,
}

#[wasm_bindgen]
impl SPARQLFormatter {
    #[wasm_bindgen(constructor)]
    pub fn new_js() -> Result<SPARQLFormatter, JsValue> {
        console_error_panic_hook::set_once();
        Self::new().map_err(|e| JsValue::from_str(&format!("filed to create SPARQL parser: {e}")))
    }

    #[wasm_bindgen(js_name = format)]
    pub fn format_js(
        &self,
        query: &str,
        prefixes: JsValue,
        indent: usize,
    ) -> Result<String, JsValue> {
        let prefixes: HashMap<String, String> = serde_wasm_bindgen::from_value(prefixes)
            .map_err(|e| JsValue::from_str(&format!("failed to parse prefixes: {e}")))?;
        let query = self
            .fix_prefixes(query, &prefixes)
            .map_err(|e| JsValue::from_str(&format!("{e}")))?;
        self.format(&query, indent)
            .map_err(|e| JsValue::from_str(&format!("{e}")))
    }
}

pub fn common_prefixes() -> HashMap<String, String> {
    [
        ("bd:", "http://www.bigdata.com/rdf#"),
        ("cc:", "http://creativecommons.org/ns#"),
        ("dct:", "http://purl.org/dc/terms/"),
        ("geo:", "http://www.opengis.net/ont/geosparql#"),
        ("hint:", "http://www.bigdata.com/queryHints#"),
        ("ontolex:", "http://www.w3.org/ns/lemon/ontolex#"),
        ("owl:", "http://www.w3.org/2002/07/owl#"),
        ("prov:", "http://www.w3.org/ns/prov#"),
        ("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("rdfs:", "http://www.w3.org/2000/01/rdf-schema#"),
        ("schema:", "http://schema.org/"),
        ("skos:", "http://www.w3.org/2004/02/skos/core#"),
        ("xsd:", "http://www.w3.org/2001/XMLSchema#"),
        ("wikibase:", "http://wikiba.se/ontology#"),
        ("wdt:", "http://www.wikidata.org/prop/direct/"),
        ("wd:", "http://www.wikidata.org/entity/"),
        ("p:", "http://www.wikidata.org/prop/"),
        ("pq:", "http://www.wikidata.org/prop/qualifier/"),
        ("ps:", "http://www.wikidata.org/prop/statement/"),
        ("pqv:", "http://www.wikidata.org/prop/qualifier/value/"),
        ("psv:", "http://www.wikidata.org/prop/statement/value/"),
    ]
    .into_iter()
    .map(|(short, long)| (short.to_string(), long.to_string()))
    .collect()
}

#[wasm_bindgen(js_name = commonSPARQLPrefixes)]
pub fn common_prefixes_js() -> Result<JsValue, JsValue> {
    Ok(serde_wasm_bindgen::to_value(&common_prefixes())?)
}

impl SPARQLFormatter {
    pub fn new() -> anyhow::Result<Self> {
        LR1GrammarParser::new(GRAMMAR, LEXER)
            .map_err(|e| anyhow!("failed to create SPARQL parser: {e}"))
            .map(|parser| Self { parser })
    }

    pub fn format(&self, query: &str, indent: usize) -> anyhow::Result<String> {
        let parse = self
            .parser
            .parse(query, true, false)
            .map_err(|_| anyhow!("failed to parse SPARQL query"))?;

        let mut current_indent = 0;
        let mut formatted = String::new();

        fn format_indent(
            parse: &LR1Parse,
            indent: usize,
            current_indent: &mut usize,
            formatted: &mut String,
        ) {
            match parse {
                LR1Parse::Empty(..) => panic!("unexpected empty node"),
                LR1Parse::Terminal(name, .., value) => {
                    if *name == "}" {
                        *current_indent -= indent;
                        *formatted = formatted.trim_end().to_string();
                        formatted.push('\n');
                        formatted.extend((0..*current_indent).map(|_| ' '));
                    }

                    formatted.push_str(String::from_utf8_lossy(value).to_string().as_str());

                    if ["}", "."].contains(name) {
                        formatted.push('\n');
                        formatted.extend((0..*current_indent).map(|_| ' '));
                    } else if *name == "{" {
                        formatted.push('\n');
                        *current_indent += indent;
                        formatted.extend((0..*current_indent).map(|_| ' '));
                    } else {
                        formatted.push(' ');
                    }
                }
                LR1Parse::NonTerminal(name, .., children) => {
                    for child in children.iter() {
                        format_indent(child, indent, current_indent, formatted);
                    }

                    if [
                        "PrefixDecl",
                        "BaseDecl",
                        "TriplesBlock",
                        "GroupClause",
                        "HavingClause",
                        "OrderClause",
                        "LimitClause",
                        "OffsetClause",
                        "GraphPatternNotTriples",
                    ]
                    .contains(name)
                    {
                        *formatted = formatted.trim_end().to_string();
                        formatted.push('\n');
                        formatted.extend((0..*current_indent).map(|_| ' '));
                    }
                }
            }
        }

        format_indent(&parse, indent, &mut current_indent, &mut formatted);
        Ok(formatted.trim().to_string())
    }

    pub fn fix_prefixes(
        &self,
        query: &str,
        prefixes: &HashMap<String, String>,
    ) -> anyhow::Result<String> {
        let mut parse = self
            .parser
            .parse(query, false, true)
            .map_err(|_| anyhow!("failed to parse SPARQL query"))?;

        let no_skip = HashSet::new();

        let mut exist = HashMap::new();
        for prefix_decl in find_all(&parse, "PrefixDecl", &no_skip) {
            let LR1Parse::NonTerminal(.., children) = prefix_decl else {
                continue;
            };
            let [_, LR1Parse::Terminal(.., short), LR1Parse::Terminal(.., long)] = &children[..]
            else {
                continue;
            };
            exist.insert(
                String::from_utf8_lossy(short).to_string(),
                String::from_utf8_lossy(long).to_string(),
            );
        }

        let mut seen = HashSet::new();
        for iri in find_all_mut(&mut parse, "IRIREF", &no_skip) {
            let LR1Parse::Terminal(.., ref mut value) = iri else {
                continue;
            };
            let iri = &value[1..value.len() - 1];
            // find longest matching prefix
            let Some((short, long)) = prefixes
                .iter()
                .filter(|&(_, long)| iri.starts_with(long.as_bytes()))
                .max_by_key(|(_, long)| long.len())
            else {
                continue;
            };
            *value = short
                .as_bytes()
                .iter()
                .chain(&iri[long.len()..])
                .copied()
                .collect();
            seen.insert(short.clone());
        }

        for prefix_name in find_all(&parse, "PNAME_LN", &no_skip) {
            let LR1Parse::Terminal(.., short) = prefix_name else {
                continue;
            };
            // find position of : in short
            let Some(pos) = short.iter().position(|&c| c == b':') else {
                continue;
            };
            seen.insert(String::from_utf8_lossy(&short[..=pos]).to_string());
        }

        let mut base_decls: Vec<_> = find_all(&parse, "BaseDecl", &no_skip).cloned().collect();

        for short in seen {
            let Some(long) = exist.get(&short).or_else(|| prefixes.get(&short)) else {
                continue;
            };
            base_decls.push(LR1Parse::NonTerminal(
                "PrefixDecl",
                vec![
                    LR1Parse::Terminal("PREFIX", (0, 0), b"PREFIX".to_vec()),
                    LR1Parse::Terminal("PNAME_NS", (0, 0), short.as_bytes().to_vec()),
                    LR1Parse::Terminal(
                        "IRIREF",
                        (0, 0),
                        (String::from("<") + long + ">").into_bytes(),
                    ),
                ],
            ));
        }

        let Some(prologue) = find_mut(&mut parse, "Prologue", &no_skip) else {
            return Err(anyhow!("failed to find prologue"));
        };
        *prologue = LR1Parse::NonTerminal("Prologue", base_decls);

        Ok(parse.flatten())
    }
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use super::*;

    #[test]
    fn test_fix_prefixes() {
        let prefixes = [("test:", "http://test.com/")]
            .into_iter()
            .map(|(short, long)| (short.to_string(), long.to_string()))
            .collect();
        let formatter = SPARQLFormatter::new().unwrap();
        let query = "PREFIX bla: <unrelated> SELECT ?x WHERE { ?x <http://test.de/prop> ?y }";
        let fixed_query = formatter.fix_prefixes(query, &prefixes).unwrap();
        assert_eq!(
            fixed_query,
            "SELECT ?x WHERE { ?x <http://test.de/prop> ?y }"
        );
        let query = "SELECT ?x WHERE { ?x test:prop ?y }";
        let fixed_query = formatter.fix_prefixes(query, &prefixes).unwrap();
        assert_eq!(
            fixed_query,
            "PREFIX test: <http://test.com/> SELECT ?x WHERE { ?x test:prop ?y }"
        );
        let query = "SELECT ?x WHERE { ?x <http://test.com/prop> ?y }";
        let fixed_query = formatter.fix_prefixes(query, &prefixes).unwrap();
        assert_eq!(
            fixed_query,
            "PREFIX test: <http://test.com/> SELECT ?x WHERE { ?x test:prop ?y }"
        );
    }

    #[test]
    fn test_format() {
        let formatter = SPARQLFormatter::new().unwrap();
        let query = "SELECT ?x WHERE { ?x <http://test.de/prop> ?y }";
        let formatted_query = formatter.format(query, 4).unwrap();
        assert_eq!(
            formatted_query,
            r#"
SELECT ?x WHERE {
    ?x <http://test.de/prop> ?y
}
"#
            .trim()
        );

        let query =
            "SELECT ?x WHERE { ?x <http://test.de/prop> ?y . ?x <http://test.de/prop2> ?z }";
        let formatted_query = formatter.format(query, 4).unwrap();
        assert_eq!(
            formatted_query,
            r#"
SELECT ?x WHERE {
    ?x <http://test.de/prop> ?y .
    ?x <http://test.de/prop2> ?z
}
"#
            .trim()
        );

        let query =
            "SELECT ?x WHERE { ?x <http://test.de/prop> ?y . FILTER(LANG(?y) = 'en') ?x <http://test.de/prop2> ?z } ORDER BY DESC( ?x)";
        let formatted_query = formatter.format(query, 4).unwrap();
        assert_eq!(
            formatted_query,
            r#"
SELECT ?x WHERE {
    ?x <http://test.de/prop> ?y .
    FILTER ( LANG ( ?y ) = 'en' )
    ?x <http://test.de/prop2> ?z
}
ORDER BY DESC ( ?x )
"#
            .trim()
        );
    }

    #[test]
    fn test_wikidata_examples() {
        let formatter = SPARQLFormatter::new().unwrap();
        // read samples from wikidata.tsv in tests directory
        let dir = env!("CARGO_MANIFEST_DIR");
        let path = format!("{}/tests/wikidata.tsv", dir);
        let file = read_to_string(path).unwrap();
        for line in file.lines() {
            let [question, sparql] = line.split('\t').collect::<Vec<_>>()[..] else {
                panic!("invalid line in wikidata.tsv: {line}");
            };
            // replace all new lines in sparql
            let sparql = sparql.replace('\n', " ");
            println!("question:\n{question}\n");
            println!("sparql:\n{sparql}\n");
            let Ok(formatted_query) = formatter.format(&sparql, 4) else {
                continue;
            };
            println!("formatted:\n{formatted_query}");
            println!("{}", (0..80).map(|_| '-').collect::<String>());
        }
    }
}
