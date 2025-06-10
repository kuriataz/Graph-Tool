# Graph-Tool

Authors: Ryszard Chereźniak, Zofia Kuriata

1. Rules:

   1. Graphs are undirected and unweighted.
   2. Vertices are distinguished by id; ids are positive natural Ints.
   3. The structure is represented by adjacency lists.
   4. Adjacency list of the vertex is its field.
   5. Adjacency list is a list of neighbours' ids.
   6. Graph can be read from file or can be created.

2. Format of the input file:

   5
   1 2
   2 3
   3 4
   4 5

   First line: number of vertices
   Next lines: pairs of adjacent vertices.

3. Tools:

   1. Finding connected components.
   2. Finding diamaters of the components.
   3. Min / Max degree.
   4. Statistics of deegres.
   5. Statistics of distances between vertices.
   6. Clustering coefficients (local and global).
   7. Counting the number of vertices and edges in the graph.
   8. Reading a graph from file.
   9. Writing a graph to file or printing it.
   10. Creating a graph with provided list of edges and auto-building some basic graphs (Kn, Kmn, Cn, empty graph, Petersen graph)
   11. Composing graphs into one graph; adding new vertices and edges

4. Usage:

In order to use the graph tool, it is necessary to first load the module from GraphAnalysis.hs file (for example in GHCI), exporting all the facilities. Standard way of printing the graphs is difficult to read, so it's advised to do it using the printGraph function instead.

A graph can be loaded from a textfile using the buildGraphFromFile function; the file must conform to the scheme described above - all the input that won't match that format will be discarded. It is also a format used by the function writeGraphToFile.

It is necessary to remember that a graph loaded from a file will unevitably be part of the IO monad. In such case, all the one-argument functions, other than IO-functions, should be used as an argument of toIO function (toIO :: (a->b) -> IO(a) -> IO(b)), meanwhile printing and writing a graph to file require usage of separate functions printIOGraph and writeIOGraphToFile.