module comp_d.LRTable;

import comp_d.Set, comp_d.tool, comp_d.data;
import std.typecons;
import std.array, std.container;
import std.algorithm, std.algorithm.comparison;
import std.stdio: writeln;
import std.conv: to;

enum Action : byte { error = 0, accept = 1, shift = 2, reduce = 3, goto_ = 4 }
alias State = size_t;
alias LREntry = Tuple!(Action, "action", State, "num");
alias LREntrySet = Set!(LREntry, (a,b) => a.action < b.action || (a.action == b.action && a.num < b.num) );
alias EntryIndex = Tuple!(State, "state", Symbol, "symbol");
alias EntryIndexSet = Set!(EntryIndex, (a,b) => a.state < b.state || (a.state == b.state && a.symbol < b.symbol));

unittest {
	enum : Symbol {
        Expr, Term, Factor,
        digit, add, mul, lPar, rPar
    }
    auto grammar_info = new GrammarInfo([
        rule(Expr, Expr, add, Term),
        rule(Expr, Term),
        rule(Term, Term, mul, Factor),
        rule(Term, Factor),
        rule(Factor, digit),
        rule(Factor, lPar, Expr, rPar),
    ], ["Expr", "Term", "Factor", "digit", "+", "*", "(", ")"]);
    
    import comp_d.SLR;
    auto table_info = SLRtableInfo(grammar_info);
    writeln(table_data(grammar_info, table_info));
    writeln("## LRTable.d");
}

// Starting state is supposed to be 0.
// (indeed, SLR/LALR/LRtableInfo returns a LRTableInfo whose starting state is 0.)
class LRTable {
    private Symbol max_symbol_number;
    public pure State  state_num() @property inout {
        return table.length;
    }
    
    public LREntry[][] table;
    
    pure this(State state_num, Symbol msyn) {
        this.max_symbol_number = msyn;
        // reserve
        table.length = state_num;
        // initialize
        foreach (state; 0 .. state_num) 
            table[state].length = msyn+2;    // consider end_of_file_ = -2
    }
    
    package pure void addState() {
        table.length += 1;
        table[$-1].length = max_symbol_number+2;
    }
    
    // return the index of symbol accessing index
    private pure size_t get_index(Symbol symbol) inout const {
        size_t access = void;
        if      (symbol == -2) access = 0;
        else if ( !( (0 <= symbol) && (symbol <= max_symbol_number) ) )
            assert(0, "Bug that should be fixed. LRTable.opIndex( ..., " ~ to!string(symbol) ~ " )");
        else                   access = symbol+1;
        return access;
    }
    
    // table[state, symbol]
    public pure LREntry opIndex(State state, Symbol symbol) inout const {
        auto access = get_index(symbol);
        return table[state][access];
    }
    // table[state, symbol]
    public pure LREntry opIndexAssign(LREntry value, State state, Symbol symbol) {
        auto access = get_index(symbol);
        return table[state][access] = value;
    }
}

// LR parser table Info
// considering the shift-reduce and reduce-reduce conflict
class LRTableInfo {
    // data[state][symbol+special_tokens]
    // see LRTable.opIndex
    private LREntrySet[][] set_data;    // for confliction
    public  LRTable table;               // if there were no confliction, this will be the LRTable of the grammar
    
    //public  bool is_conflict;
    
    private EntryIndexSet conflict_index_set;
    package pure bool is_conflict() @property inout const {
        return conflict_index_set.cardinal > 0;
    }
    package pure bool is_conflicting(State state, Symbol symbol) inout const {
        return EntryIndex(state, symbol) in conflict_index_set;
    }
    
    package pure inout(EntryIndex)[] conflictings() @property inout const {
        return conflict_index_set.array;
    }
    
    private State state_number;
    package pure State state_num() @property inout const {
        return state_number;
    }
    
    pure this(State state_num, Symbol msyn) {
        // init
        table = new LRTable(state_num, msyn);
        // reserve
        set_data.length = state_num; state_number = state_num;
        // initialize
        foreach (state; 0 .. state_num) {
            set_data[state].length = msyn+2;    // consider end_of_file_ = -2
            foreach (symbol; 0 .. msyn+2) set_data[state][symbol] = new LREntrySet();
        }
        conflict_index_set = new EntryIndexSet();
    }
    
    package pure void addState() {
        table.addState();
        set_data.length += 1;
        set_data[$-1].length = table.max_symbol_number+2;
        foreach (symbol; 0 .. table.max_symbol_number+2) set_data[$-1][symbol] = new LREntrySet();
    }
    
    // return the set
    // table[state, symbol]
    public pure LREntrySet opIndex(State state, Symbol symbol) inout const {
        auto access = table.get_index(symbol);
        return cast(LREntrySet) set_data[state][access];
    }
    
    // determine the value
    // table[state, symbol] = Entry
    public pure void opIndexAssign(LREntry value, State state, Symbol symbol) {
        auto access = table.get_index(symbol);
        // determine the entry
        table[state, symbol] = value;
        conflict_index_set.remove(EntryIndex(state, symbol));
    }
    
    /* if conflicting occurs on the symbol 'symbol', solve it by replacing by action(reduce or shift)
    public void opIndexAssign(Action action, Symbol symbol) {
        
    }
     */
    
    // add entry to the table[state, symbol]
    package pure void add(LREntry value, State state, Symbol symbol) {
        auto access = table.get_index(symbol);
        set_data[state][access].add(value);
        // conflicted
        if (table[state, symbol].action != Action.error && table[state, symbol] != value) {
            conflict_index_set.add(EntryIndex(state, symbol));
            // when shift-reduce conflict occurs, shift is chosen. 
            if (value.action == Action.shift) table[state, symbol] = value;
        }
        else table[state, symbol] = value;
    }
}

// return an AATree[symbol][state] = tuple(Action.***, number);
pure string table_data(inout GrammarInfo grammar_info, inout LRTableInfo table_info) {

	string result = "new AATree!(long /+ symbol +/, (a,b)=>a<b, LREntry[])(\n";
	auto state_num = table_info.table.state_num, symbol_num = grammar_info.max_symbol_num;
	
	foreach (sym; 0 .. symbol_num) {
		if (sym !in grammar_info.terminals) continue;
		result ~= "\ttuple(" ~ sym.to!string ~ ", [" ;
		foreach (st; 0 .. state_num) {
			auto act = table_info.table[st, sym].action;
			auto act_str =
				act == Action.error  ? "error" :
				act == Action.shift  ? "shift" :
				act == Action.reduce ? "reduce":
				act == Action.accept ? "accept" : "";
				
			result ~= "LREntry(Action." ~ act_str ~ ", " ~ table_info.table[st, sym].num.to!string ~ "), ";
		}
		result ~= "]),\n";
	}
	
	return result ~ ")";
}

