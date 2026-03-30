const svg = d3.select("svg");
const sizx = +svg.attr("sizx");
const sizy = +svg.attr("sizy");
const file = svg.attr("file");

svg.attr("style", "max-width: 100%; height: auto;");
svg.attr("viewBox", [-sizx / 2, -sizy / 2, sizx, sizy])

d3.json(file).then(function (data) {

const sim = d3.forceSimulation(data.nodes)
  .force("charge", d3.forceManyBody())
  .force("link", d3.forceLink(data.links).id(d => d.id))
  .force("center", d3.forceCenter());

const link = svg.append("g")
  .attr("stroke", "#999")
  .attr("stroke-opacity", 0.6)
  .selectAll("line")
  .data(data.links)
  .join("line");

const node = svg.append("g")
  .attr("stroke", "#fff")
  .attr("stroke-width", 1.5)
  .selectAll("circle")
  .data(data.nodes)
  .join("circle")
  .attr("r", 4);

const drag = d3.drag();

drag.on("start", (e) => {
  if (!e.active) sim.alphaTarget(0.3).restart();
  e.subject.fx = e.subject.x;
  e.subject.fy = e.subject.y;
});

drag.on("drag", (e) => {
  e.subject.fx = e.x;
  e.subject.fy = e.y;
})

drag.on("end", (e) => {
  if (!e.active) sim.alphaTarget(0);
  e.subject.fx = null;
  e.subject.fy = null;
});

node.append("title").text(d => d.id);

node.call(drag);

sim.on("tick", () => {
  link.attr("x1", d => d.source.x).attr("y1", d => d.source.y);
  link.attr("x2", d => d.target.x).attr("y2", d => d.target.y);
  node.attr("cx", d => d.x).attr("cy", d => d.y);
});

});
