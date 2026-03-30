const graph = new ForceGraph3D(document.getElementById('3d-graph'))
  .jsonUrl("graph.tcl")
  .nodeAutoColorBy('group')
  .nodeLabel(node => node.id)
  .linkOpacity(1)
  .nodeThreeObjectExtend(true)
  .d3Force('charge')
  .strength(-120);
