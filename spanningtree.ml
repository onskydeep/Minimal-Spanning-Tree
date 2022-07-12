type graph = (int * float * int) list;;

let rec length list=
  match list with
  | [] -> 0
  | x::xs -> length xs + 1

let rec contains_edge edge subgraph =
  match subgraph with
  | [] -> false
  | subedge::edges -> (
      match (edge,subedge) with
      | ((a,_,b),(c,_,d)) -> (if (a==c && b==d) then true else contains_edge edge edges)
    )
    
let rec contains vertex list =
  match list with
  | [] -> false
  | element::elements -> if vertex==element then true else contains vertex elements
          
          

let not exp = 
  match exp with
  |false -> true
  |true -> false
    
    

let rec vertices_in_graph_help1 graph seen_vertices =
  match graph with
  | [] -> seen_vertices
  | (edge::edges) -> (
      match edge with
      | (a,_,_) -> (
          if not (contains a seen_vertices) then 
            vertices_in_graph_help1 edges (seen_vertices @  [a])
          else 
            vertices_in_graph_help1 edges seen_vertices
        )
    )
    
let rec vertices_in_graph_help2 graph seen_vertices = 
  match graph with
  | [] -> seen_vertices
  | (edge::edges) -> (
      match edge with
      | (_,_,b) -> (
          if not (contains b seen_vertices) then 
            vertices_in_graph_help2 edges (seen_vertices @  [b])
          else 
            vertices_in_graph_help2 edges seen_vertices
        )
    )
    
let rec vertices_in_graph graph =
  vertices_in_graph_help2 graph (vertices_in_graph_help1 graph [])
    
    
let rec vertices_total graph = 
  length (vertices_in_graph graph)
    
let isempty graph =
  if graph == [] then true
  else false

let rec connects edge subgraph vertices_subgraph=
  
  if isempty subgraph then true
    
  else(
    match edge with
    | (a,_,b) -> (
        if ( ( (not (contains a vertices_subgraph) ) && (contains b vertices_subgraph) ) ||
             ( (contains a vertices_subgraph) && (not (contains b vertices_subgraph))) ) then true
        else false
      )
  ) 
    
let rec minimal_edge_among_left subgraph graph min_edge =
  match graph with
  | [] -> min_edge
  | (edge::edges) -> (
      match (edge,min_edge) with
      |((_,price,_),(_,min_price,_))  -> (
          if (price<min_price && connects edge subgraph (vertices_in_graph subgraph)) 
          then minimal_edge_among_left subgraph edges edge 
          else minimal_edge_among_left subgraph edges min_edge 
        ) 
    )   
  
let rec build_mst graph vertices_num subgraph_mst iteration =
  if iteration == vertices_num then subgraph_mst
  else (
    build_mst graph vertices_num ( subgraph_mst @ [minimal_edge_among_left subgraph_mst graph (0,1073741823.,0)]) (iteration + 1)
  )

let rec mst graph =
  build_mst graph (vertices_total graph) [] 1
    
  

    
    

    
    

  
  
  
  
  

             
