
public class Circular {
    public int val;
    public Circular env;

    public static void main(String[] args) {
        Circular a = new Circular();
        a.val = 42;

        Circular b = new Circular();
        b.val = 24;

        a.env = b;
        b.env = a;

        System.out.println(a.val);
        System.out.println(a.env.val);
    }

    public String toString() {
        //return String.format("val: %d", val);
        return String.format("val: %d, env: %s", val, env);
    }
}
