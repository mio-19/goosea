package goosea.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.joou.UInteger;
import org.jetbrains.annotations.NotNull;
import org.joou.ULong;
import org.joou.UInteger;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;

@NodeInfo(language = "Goosea", description = "The node")
public class GooseaNode extends GooseaAbstractNode {
    // unsigned
    public final long pc;
    // unsigned
    @CompilationFinal
    private int instr;

    public ULong getPC() {
        return ULong.valueOf(this.pc);
    }

    private UInteger getInstr() {
        return UInteger.valueOf(this.instr);
    }

    public GooseaNode(long pc) {
        this.pc = pc;
    }

    @Override
    public void execute(VirtualFrame frame) {
        Context context = getContext();
        int currentInstr = context.cpu().fetchForMock(ULong.valueOf(this.pc)).intValue();
        if (currentInstr != this.instr) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var lock = getLock();
            lock.lock();
            this.instr = currentInstr;
            lock.unlock();
        }
        context.cpu().mockTick(this.getPC(), this.getInstr());
    }

    public final Context getContext() {
        return Context.get(this);
    }
}
