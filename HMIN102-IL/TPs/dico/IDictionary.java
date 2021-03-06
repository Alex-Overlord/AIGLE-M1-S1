package dico;

public interface IDictionary {
	int size();
	Object get(Object key);
	IDictionary put(Object key, Object value);
	boolean isEmpty();
	boolean containsKey(Object key);
}