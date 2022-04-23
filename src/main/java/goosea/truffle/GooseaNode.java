package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.joou.UInteger;
import org.jetbrains.annotations.NotNull;
import org.joou.ULong;
import com.oracle.truffle.api.dsl.*;

@NodeInfo(language = "Goosea", description = "The node")
public abstract class GooseaNode extends GooseaAbstractNode {
    // unsigned
    public final long pc;

    public ULong getPC() {
        return ULong.valueOf(this.pc);
    }

    public GooseaNode(long pc) {
        this.pc = pc;
    }

    @Specialization
    protected Object runInstr() {
        Context context = getContext();
        context.cpu().mockTick(this.getPC());
        return null;
    }

    @Override
    public String toString() {
        return "GooseaNode{" +
                "pc=" + this.getPC() +
                '}';
    }

    public final Context getContext() {
        return Context.get(this);
    }
}
