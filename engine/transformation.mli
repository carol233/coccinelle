val transform: 
 Lib_engine.transformation_info -> Control_flow_c.cflow -> Control_flow_c.cflow


val transform_proto :
  Ast_cocci.rule_elem ->
  Control_flow_c.node ->
  Lib_engine.metavars_binding ->
  Ast_c.typeQualifier * Ast_c.info -> 
  string Ast_cocci.mcode -> 
  Ast_c.programElement

exception NoMatch 

