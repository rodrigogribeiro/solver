List new_node(List anchor) {
  List node = (List)malloc(12);
  node->next = anchor;
  return node;
}