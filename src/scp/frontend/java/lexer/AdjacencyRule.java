package scp.frontend.java.lexer;

public class AdjacencyRule {
    private int[] denseMap;
    private char[] sparseCharacters;
    private int[] sparseMap;

    /**
     * Unpacks a run-length-encoded array.
     *
     * In a run-length-encoded array, the elements at index 2*i are a
     * count of the number of elements with value at index 2*i+1. To
     * unpack the original array, these integer pairs are expanded in
     * order.
     *
     * Example:
     *
     *     packed: { 2, 7, 1, 8 }
     *
     *             "the original array was 2 instances of 7 and then 1
     *             instance of 8"
     *
     *     unpacked: { 7, 7, 8 }
     * 
     */
    private int[] unpackRunLengthEncoding(int[] rle) {
	int count = 0;

	for (int i = 0; i < rle.length; i += 2)
	    count += rle[i];

	int[] result = new int[count];
	int index = 0;
	for (int i = 0; i < rle.length; i+= 2)
	    for (int instance = 0; instance < rle[i]; instance++)
		result[index++] = rle[i+1];

	return result;
    }

    /**
     * Constructs a new AdjacencyRule object given a fixed, dense
     * mapping of characters to nodes, and a sparse mapping.
     *
     * The fixed, dense array is given in a run-length encoding.  It
     * is an even-sized array of integer pairs where the first integer
     * is a count of the number of instances of the second integer.
     * These pairs are given in the original order of the array.
     *
     * If the numeric value of a given character is less than the size
     * of the denseMap array, then the value of that character is used
     * as an index into the array to determine the result of the rule.
     *
     * Otherwise, the sparseCharacters array is searched, and if
     * found, the sparseMap is used as a parallel array so that if the
     * index i in sparseChracters where the input is found, then
     * sparseMap[i] is the result of the rule.
     *
     * The sparseCharacters map should be sorted in ascending order to
     * facilitate a fast binary search lookup.
     */
    public AdjacencyRule(int[] runLengthDenseMap, char[] sparseCharacters, int[] sparseMap) {
        if ((sparseCharacters != null) && sparseCharacters.length != sparseMap.length)
            throw new RuntimeException();

        this.denseMap = unpackRunLengthEncoding(runLengthDenseMap);
        this.sparseCharacters = sparseCharacters;
        this.sparseMap = sparseMap;
    }

    public AdjacencyRule(int[] runLengthDenseMap) {
	this(runLengthDenseMap, null, null);
    }

    /**
     * Applies the result of the rule on the specified input.
     */
    public int lookup(char c) {
        /**
         * If the rule falls in the dense map, perform a direct
         * lookup.
         */
        if (c >= 0 && c < denseMap.length)
            return denseMap[c];

        /**
         * Otherwise, perform a search.
         * TODO Replace this with a binary search.
         */
	if (sparseCharacters != null)
	    for (int i = 0; i < sparseCharacters.length; i++)
		if (sparseCharacters[i] == c)
		    return sparseMap[i];

        return -1;
    }
}
