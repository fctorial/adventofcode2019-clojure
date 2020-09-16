package proj.java;

public class Return extends Throwable {
    public final Object value;

    public Return(Object value) {
        this.value = value;
    }
}
