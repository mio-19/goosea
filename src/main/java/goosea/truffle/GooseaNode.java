package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.joou.UInteger;
import org.jetbrains.annotations.NotNull;
import org.joou.ULong;

@NodeInfo(language = "Goosea", description = "The node")
public class GooseaNode extends Node {
    public final int instr;

    public GooseaNode(int instr) {
        this.instr = instr;
    }

    public GooseaNode(UInteger instr) {
        this.instr = instr.intValue();
    }

    public UInteger getInstrU() {
        return UInteger.valueOf(instr);
    }

    public void execute(VirtualFrame frame) {
        throw new UnsupportedOperationException("TODO");
    }

    @Override
    public String toString() {
        return "GooseaNode{" +
                "instr=" + instr +
                '}';
    }
}
