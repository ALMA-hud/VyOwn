use strict;
no warnings;

my @KEYWORDS = qw(start end egg een set ram for see put per way due lap if or tie);
my %KW = map { $_ => uc($_) } @KEYWORDS;

sub extract_ties {
    my ($src) = @_;
    my @ties;
    my $out = '';
    my $pos = 0;
    while ($src =~ /\btie\s*\{/g) {
        my $matchEnd = pos($src);
        $out .= substr($src, $pos, $-[0] - $pos);
        my $depth = 1;
        my $j = $matchEnd;
        my $n = length($src);
        while ($j < $n && $depth > 0) {
            my $c = substr($src,$j,1);
            $depth++ if $c eq '{';
            $depth-- if $c eq '}';
            last if $depth == 0;
            $j++;
        }
        my $content = substr($src, $matchEnd, $j - $matchEnd);
        push @ties, $content;
        $out .= " TIEBLOCK" . $#ties . " ";
        $pos = $j + 1;
        pos($src) = $pos;
    }
    $out .= substr($src, $pos);
    return ($out, \@ties);
}

sub strip_comments {
    my ($src) = @_;
    $src =~ s/~[^\n]*//g;
    my $out = '';
    my $in_comment = 0;
    for my $c (split //, $src) {
        if ($c eq '*') {
            $in_comment = $in_comment ? 0 : 1;
            next;
        }
        $out .= $c unless $in_comment;
    }
    return $out;
}

sub tokenize {
    my ($src) = @_;
    my @tokens;
    my $i = 0;
    my $n = length($src);
    while ($i < $n) {
        my $c = substr($src,$i,1);
        if ($c =~ /\s/) { $i++; next }
        if ($c eq ',') { push @tokens, ['COMMA',',']; $i++; next }
        if ($c eq '(') { push @tokens, ['LP','(']; $i++; next }
        if ($c eq ')') { push @tokens, ['RP',')']; $i++; next }
        if ($c eq '{') { push @tokens, ['LB','{']; $i++; next }
        if ($c eq '}') { push @tokens, ['RB','}']; $i++; next }
        if (substr($src,$i,2) eq '==') { push @tokens, ['EQEQ','==']; $i+=2; next }
        if ($c eq '=') { push @tokens, ['EQ','=']; $i++; next }
        if ($c eq '+') { push @tokens, ['PLUS','+']; $i++; next }
        if ($c eq '-') { push @tokens, ['MINUS','-']; $i++; next }
        if ($c eq '*') { push @tokens, ['STAR','*']; $i++; next }
        if ($c eq '/') { push @tokens, ['SLASH','/']; $i++; next }
        if ($c eq '>') { push @tokens, ['GT','>']; $i++; next }
        if ($c eq '<') { push @tokens, ['LT','<']; $i++; next }
        if (substr($src,$i) =~ /^([A-Za-z0-9_]+)/) {
            my $word = $1;
            if (exists $KW{$word}) {
                push @tokens, [$KW{$word}, $word];
            } else {
                push @tokens, ['WORD', $word];
            }
            $i += length($word);
            next;
        }
        push @tokens, ['WORD', $c];
        $i++;
    }
    push @tokens, ['EOF',''];
    return \@tokens;
}

my @TOKS;
my $POS;
sub peek_type { return $TOKS[$POS][0] }
sub advance { return $TOKS[$POS++] }
sub expect {
    my ($t) = @_;
    die unless $TOKS[$POS][0] eq $t;
    return $TOKS[$POS++];
}

sub parse_program {
    expect('START');
    my @stmts;
    while (peek_type() ne 'END' && peek_type() ne 'EOF') {
        push @stmts, parse_statement();
    }
    expect('END');
    return { type=>'program', body=>\@stmts };
}

sub parse_operand {
    my $t = peek_type();
    if ($t eq 'WAY') {
        advance();
        my $addr = advance()->[1];
        return { type=>'way', addr=>$addr };
    }
    if ($t eq 'SEE') {
        advance();
        return parse_operand();
    }
    if ($t eq 'WORD') {
        my $v = advance()->[1];
        return { type=>'ref', value=>$v };
    }
    die;
}

sub parse_condition {
    my $a = parse_operand();
    my $t = peek_type();
    if ($t eq 'EQ' || $t eq 'EQEQ' || $t eq 'GT' || $t eq 'LT') {
        advance();
        my $b = parse_operand();
        return { a=>$a, b=>$b, op=>$t };
    }
    return { a=>$a, b=>undef, op=>undef };
}

sub parse_egg_item {
    my $value = advance()->[1];
    my $addr;
    if (peek_type() eq 'EEN') {
        advance();
        $addr = advance()->[1];
    }
    my $name;
    if (peek_type() eq 'SET') {
        advance();
        $name = advance()->[1];
    }
    return { value=>$value, addr=>$addr, name=>$name };
}

sub parse_egg {
    expect('EGG');
    my @items;
    push @items, parse_egg_item();
    while (peek_type() eq 'COMMA') {
        advance();
        push @items, parse_egg_item();
    }
    return { type=>'egg', items=>\@items };
}

sub parse_ram {
    expect('RAM');
    my $addr = advance()->[1];
    my $value = '0';
    if (peek_type() eq 'FOR') {
        advance();
        $value = advance()->[1];
    }
    my $name;
    if (peek_type() eq 'SET') {
        advance();
        $name = advance()->[1];
    }
    return { type=>'ram', addr=>$addr, value=>$value, name=>$name };
}

sub parse_put {
    expect('PUT');
    my @items;
    if (peek_type() eq 'LP') {
        advance();
        push @items, parse_operand();
        while (peek_type() eq 'COMMA') {
            advance();
            push @items, parse_operand();
        }
        expect('RP');
    } else {
        push @items, parse_operand();
        while (peek_type() eq 'COMMA') {
            advance();
            push @items, parse_operand();
        }
    }
    expect('PER');
    my $target = advance()->[1];
    if (peek_type() eq 'SET') {
        advance();
        advance();
    }
    return { type=>'put', items=>\@items, target=>$target };
}

sub parse_way_stmt {
    expect('WAY');
    my $addr = advance()->[1];
    my $name;
    if (peek_type() eq 'SET') {
        advance();
        $name = advance()->[1];
    }
    return { type=>'way_stmt', addr=>$addr, name=>$name };
}

sub parse_due {
    expect('DUE');
    my $addr;
    if (peek_type() ne 'LB') {
        $addr = advance()->[1];
    }
    expect('LB');
    my @stmts;
    while (peek_type() ne 'RB') {
        push @stmts, parse_statement();
    }
    expect('RB');
    return { type=>'due', addr=>$addr, body=>\@stmts };
}

sub parse_lap {
    my ($count) = @_;
    expect('LAP');
    expect('LB');
    my @stmts;
    while (peek_type() ne 'RB') {
        push @stmts, parse_statement();
    }
    expect('RB');
    return { type=>'lap', count=>$count, body=>\@stmts };
}

sub parse_if {
    expect('IF');
    my @clauses;
    my $cond = parse_condition();
    expect('LB');
    my @body;
    while (peek_type() ne 'RB') {
        push @body, parse_statement();
    }
    expect('RB');
    push @clauses, { cond=>$cond, body=>\@body };
    while (peek_type() eq 'OR') {
        advance();
        my $c2;
        if (peek_type() ne 'LB') {
            $c2 = parse_condition();
        }
        expect('LB');
        my @b2;
        while (peek_type() ne 'RB') {
            push @b2, parse_statement();
        }
        expect('RB');
        push @clauses, { cond=>$c2, body=>\@b2 };
    }
    return { type=>'if', clauses=>\@clauses };
}

sub parse_statement {
    my $t = peek_type();
    if ($t eq 'WORD' && $TOKS[$POS][1] =~ /^TIEBLOCK(\d+)$/) {
        advance();
        return { type=>'tie', idx=>$1 };
    }
    if ($t eq 'EGG') { return parse_egg() }
    if ($t eq 'RAM') { return parse_ram() }
    if ($t eq 'PUT') { return parse_put() }
    if ($t eq 'WAY') { return parse_way_stmt() }
    if ($t eq 'DUE') { return parse_due() }
    if ($t eq 'IF') { return parse_if() }
    if ($t eq 'LAP') { return parse_lap(undef) }
    if ($t eq 'WORD' && $TOKS[$POS+1][0] eq 'LAP') {
        my $count = advance()->[1];
        return parse_lap($count);
    }
    if ($t eq 'WORD' && $TOKS[$POS+1][0] eq 'SEE') {
        my $name = advance()->[1];
        advance();
        my $operand = parse_operand();
        return { type=>'aliasput', name=>$name, value=>$operand };
    }
    my $operand1 = parse_operand();
    my $optype = peek_type();
    if ($optype eq 'PLUS' || $optype eq 'MINUS' || $optype eq 'STAR' || $optype eq 'SLASH') {
        advance();
        my $operand2 = parse_operand();
        if (peek_type() eq 'EQ') {
            advance();
        } elsif (peek_type() eq 'SET') {
            advance();
        } else {
            die;
        }
        my $name = advance()->[1];
        return { type=>'arith', op=>$optype, a=>$operand1, b=>$operand2, dest=>$name };
    }
    if ($optype eq 'EQ' || $optype eq 'EQEQ' || $optype eq 'GT' || $optype eq 'LT') {
        advance();
        my $operand2 = parse_operand();
        my $name;
        if (peek_type() eq 'SET') {
            advance();
            $name = advance()->[1];
        }
        return { type=>'compare', a=>$operand1, b=>$operand2, op=>$optype, dest=>$name };
    }
    return { type=>'nop' };
}

sub is_hex { my ($v)=@_; return defined($v) && $v =~ /^0x[0-9A-Fa-f]+$/ }
sub is_num { my ($v)=@_; return defined($v) && $v =~ /^[0-9]+$/ }
sub numval { my ($v)=@_; return hex($v) if is_hex($v); return $v+0 if is_num($v); return ord(substr($v,0,1)) }
sub valtype { my ($v)=@_; return 'num' if is_hex($v) || is_num($v); return 'str' }

sub compute_devaddr {
    my ($key) = @_;
    return undef unless defined $key;
    return undef unless is_hex($key);
    my $v = hex($key);
    return $v if $v==0x1000 || $v==0x2000 || $v==0x3000 || $v==0x4000;
    return undef;
}

my %symtab;
my @data_defs;
my @code_lines;
my $gid;
my $disk_sector_counter;
my @tie_store;
my %NEGATE_JMP = ('EQ'=>'jne','EQEQ'=>'jne','GT'=>'jle','LT'=>'jge');
my %POS_JMP = ('EQ'=>'je','EQEQ'=>'je','GT'=>'jg','LT'=>'jl');

sub emitc { push @code_lines, $_[0] }
sub newlabel { return 'L' . $gid++ }
sub newsym { return 'S' . $gid++ }

sub resolve_device_addr {
    my ($tok) = @_;
    if (exists $symtab{$tok} && defined $symtab{$tok}{devaddr}) {
        return $symtab{$tok}{devaddr};
    }
    if (is_hex($tok)) {
        my $v = hex($tok);
        return $v if $v==0x1000||$v==0x2000||$v==0x3000||$v==0x4000;
    }
    return undef;
}

sub resolve_value_operand {
    my ($node) = @_;
    if ($node->{type} eq 'way') {
        emit_way_read_to_ax($node->{addr});
        my $label = newsym();
        push @data_defs, "$label: dw 0";
        emitc("mov [$label], ax");
        return "[$label]";
    }
    my $v = $node->{value};
    if (exists $symtab{$v}) {
        return "[$symtab{$v}{label}]";
    }
    if (is_hex($v)) { return hex($v) }
    if (is_num($v)) { return $v }
    return ord(substr($v,0,1));
}

sub strip_brackets {
    my ($s) = @_;
    if ($s =~ /^\[(.+)\]$/) { return $1 }
    return $s;
}

sub resolve_output_operand {
    my ($node) = @_;
    if ($node->{type} eq 'way') {
        emit_way_read_to_ax($node->{addr});
        my $label = newsym();
        push @data_defs, "$label: dw 0";
        emitc("mov [$label], ax");
        return ('num', "[$label]");
    }
    my $v = $node->{value};
    if (exists $symtab{$v}) {
        my $e = $symtab{$v};
        return ($e->{dtype}, "[$e->{label}]");
    }
    if (is_hex($v)) { return ('num', hex($v)) }
    if (is_num($v)) { return ('num', $v) }
    if (length($v) == 1) { return ('num', ord($v)) }
    my $label = newsym();
    push @data_defs, "$label: db \"$v\",0";
    return ('str', "[$label]");
}

sub data_def_line {
    my ($label, $value, $type) = @_;
    if ($type eq 'num') {
        return "$label: dw " . numval($value);
    }
    return "$label: db \"$value\",0";
}

sub store_immediate_to_entry {
    my ($entry, $value) = @_;
    if ($entry->{dtype} eq 'num') {
        emitc("mov word [$entry->{label}], " . numval($value));
    }
}

sub gen_egg {
    my ($s) = @_;
    for my $item (@{$s->{items}}) {
        my $key = $item->{addr};
        my $type = valtype($item->{value});
        my $entry;
        if (defined $key && exists $symtab{$key}) {
            $entry = $symtab{$key};
            store_immediate_to_entry($entry, $item->{value});
        } else {
            my $label = newsym();
            $entry = { label=>$label, dtype=>$type, devaddr=>compute_devaddr($key) };
            $symtab{$key} = $entry if defined $key;
            push @data_defs, data_def_line($label, $item->{value}, $type);
        }
        $symtab{$item->{name}} = $entry if defined $item->{name};
    }
}

sub gen_ram {
    my ($s) = @_;
    my $type = valtype($s->{value});
    my $entry;
    if (exists $symtab{$s->{addr}}) {
        $entry = $symtab{$s->{addr}};
        store_immediate_to_entry($entry, $s->{value});
    } else {
        my $label = newsym();
        $entry = { label=>$label, dtype=>$type, devaddr=>compute_devaddr($s->{addr}) };
        $symtab{$s->{addr}} = $entry;
        push @data_defs, data_def_line($label, $s->{value}, $type);
    }
    $symtab{$s->{name}} = $entry if defined $s->{name};
}

sub gen_arith {
    my ($s) = @_;
    my $r1 = resolve_value_operand($s->{a});
    my $r2 = resolve_value_operand($s->{b});
    emitc("mov ax, $r1");
    if ($s->{op} eq 'PLUS') {
        emitc("add ax, $r2");
    } elsif ($s->{op} eq 'MINUS') {
        emitc("sub ax, $r2");
    } elsif ($s->{op} eq 'STAR') {
        emitc("mov bx, $r2");
        emitc("mul bx");
    } elsif ($s->{op} eq 'SLASH') {
        emitc("mov bx, $r2");
        emitc("xor dx, dx");
        emitc("div bx");
    }
    my $entry;
    if (exists $symtab{$s->{dest}}) {
        $entry = $symtab{$s->{dest}};
    } else {
        my $label = newsym();
        $entry = { label=>$label, dtype=>'num' };
        $symtab{$s->{dest}} = $entry;
        push @data_defs, "$label: dw 0";
    }
    emitc("mov word [$entry->{label}], ax");
}

sub gen_compare {
    my ($s) = @_;
    return unless defined $s->{dest};
    my $r1 = resolve_value_operand($s->{a});
    emitc("mov ax, $r1");
    my $r2 = resolve_value_operand($s->{b});
    emitc("cmp ax, $r2");
    my $jmp = $POS_JMP{$s->{op}} || 'je';
    my $true_l = newlabel();
    my $end_l = newlabel();
    emitc("$jmp $true_l");
    emitc("mov ax, 0");
    emitc("jmp $end_l");
    emitc("$true_l:");
    emitc("mov ax, 1");
    emitc("$end_l:");
    my $entry;
    if (exists $symtab{$s->{dest}}) {
        $entry = $symtab{$s->{dest}};
    } else {
        my $label = newsym();
        $entry = { label=>$label, dtype=>'num' };
        $symtab{$s->{dest}} = $entry;
        push @data_defs, "$label: dw 0";
    }
    emitc("mov word [$entry->{label}], ax");
}

sub gen_branch_if_false {
    my ($cond, $false_label) = @_;
    my $r1 = resolve_value_operand($cond->{a});
    emitc("mov ax, $r1");
    if (defined $cond->{b}) {
        my $r2 = resolve_value_operand($cond->{b});
        emitc("cmp ax, $r2");
        my $negjmp = $NEGATE_JMP{$cond->{op}} || 'jne';
        emitc("$negjmp $false_label");
    } else {
        emitc("cmp ax, 0");
        emitc("je $false_label");
    }
}

sub gen_block {
    my ($stmts) = @_;
    for my $st (@$stmts) {
        gen_statement($st);
    }
}

sub gen_if {
    my ($s) = @_;
    my $end_label = newlabel();
    my @clauses = @{$s->{clauses}};
    for my $i (0..$#clauses) {
        my $clause = $clauses[$i];
        my $is_last = ($i == $#clauses);
        if (defined $clause->{cond}) {
            my $next_label = $is_last ? $end_label : newlabel();
            gen_branch_if_false($clause->{cond}, $next_label);
            gen_block($clause->{body});
            if (!$is_last) {
                emitc("jmp $end_label");
                emitc("$next_label:");
            }
        } else {
            gen_block($clause->{body});
            emitc("jmp $end_label") unless $is_last;
        }
    }
    emitc("$end_label:");
}

sub gen_lap {
    my ($s) = @_;
    my $start_label = newlabel();
    if (defined $s->{count}) {
        emitc("mov cx, " . numval($s->{count}));
        emitc("$start_label:");
        emitc("push cx");
        gen_block($s->{body});
        emitc("pop cx");
        emitc("loop $start_label");
    } else {
        emitc("$start_label:");
        gen_block($s->{body});
        emitc("jmp $start_label");
    }
}

sub emit_way_read_to_ax {
    my ($addr) = @_;
    my $dev = resolve_device_addr($addr);
    $dev = hex($addr) if !defined($dev) && is_hex($addr);
    $dev = 0 unless defined $dev;
    if ($dev == 0x2000) {
        emitc("mov ah, 0x00");
        emitc("int 0x16");
        emitc("mov ah, 0");
    } elsif ($dev == 0x3000) {
        emitc("mov ax, 0x0003");
        emitc("int 0x33");
        emitc("mov ax, bx");
    } elsif ($dev == 0x4000) {
        my $buf = newsym();
        push @data_defs, "$buf: times 512 db 0";
        emitc("mov bx, $buf");
        emitc("mov ah, 0x02");
        emitc("mov al, 1");
        emitc("mov ch, 0");
        emitc("mov cl, " . ($disk_sector_counter++));
        emitc("mov dh, 0");
        emitc("mov dl, [S_bootdrv]");
        emitc("int 0x13");
        emitc("mov ax, [$buf]");
    } else {
        emitc("mov ax, 0");
    }
}

sub emit_put_monitor {
    my ($node) = @_;
    my ($kind, $ref) = resolve_output_operand($node);
    if ($kind eq 'str') {
        my $loop = newlabel();
        my $done = newlabel();
        emitc("mov si, " . strip_brackets($ref));
        emitc("$loop:");
        emitc("lodsb");
        emitc("test al, al");
        emitc("jz $done");
        emitc("mov ah, 0x0e");
        emitc("mov bh, 0");
        emitc("int 0x10");
        emitc("jmp $loop");
        emitc("$done:");
    } else {
        emitc("mov al, $ref");
        emitc("mov ah, 0x0e");
        emitc("mov bh, 0");
        emitc("int 0x10");
    }
}

sub emit_put_disk {
    my ($node) = @_;
    my ($kind, $ref) = resolve_output_operand($node);
    my $addr_ref;
    if ($kind eq 'str') {
        $addr_ref = strip_brackets($ref);
    } else {
        my $label = newsym();
        push @data_defs, "$label: dw 0";
        emitc("mov ax, $ref");
        emitc("mov [$label], ax");
        $addr_ref = $label;
    }
    my $sector = $disk_sector_counter++;
    emitc("mov bx, $addr_ref");
    emitc("mov ah, 0x03");
    emitc("mov al, 1");
    emitc("mov ch, 0");
    emitc("mov cl, $sector");
    emitc("mov dh, 0");
    emitc("mov dl, [S_bootdrv]");
    emitc("int 0x13");
}

sub emit_put_item {
    my ($node, $dev) = @_;
    if ($dev == 0x1000) {
        emit_put_monitor($node);
    } elsif ($dev == 0x4000) {
        emit_put_disk($node);
    }
}

sub gen_put {
    my ($s) = @_;
    my $dev = resolve_device_addr($s->{target});
    return unless defined $dev;
    for my $item (@{$s->{items}}) {
        emit_put_item($item, $dev);
    }
}

sub gen_way_stmt {
    my ($s) = @_;
    emit_way_read_to_ax($s->{addr});
    if (defined $s->{name}) {
        my $entry;
        if (exists $symtab{$s->{name}}) {
            $entry = $symtab{$s->{name}};
        } else {
            my $label = newsym();
            $entry = { label=>$label, dtype=>'num' };
            $symtab{$s->{name}} = $entry;
            push @data_defs, "$label: dw 0";
        }
        emitc("mov word [$entry->{label}], ax");
    }
}

sub gen_due {
    my ($s) = @_;
    if (defined $s->{addr}) {
        my $dev = resolve_device_addr($s->{addr});
        $dev = hex($s->{addr}) if !defined($dev) && is_hex($s->{addr});
        if (defined $dev && $dev == 0x2000) {
            my $poll = newlabel();
            emitc("$poll:");
            emitc("mov ah, 0x01");
            emitc("int 0x16");
            emitc("jz $poll");
            emitc("mov ah, 0x00");
            emitc("int 0x16");
        }
    }
    gen_block($s->{body});
}

sub gen_aliasput {
    my ($s) = @_;
    my $dev = resolve_device_addr($s->{name});
    return unless defined $dev;
    emit_put_item($s->{value}, $dev);
}

sub gen_statement {
    my ($s) = @_;
    my $t = $s->{type};
    if ($t eq 'egg') { gen_egg($s) }
    elsif ($t eq 'ram') { gen_ram($s) }
    elsif ($t eq 'arith') { gen_arith($s) }
    elsif ($t eq 'compare') { gen_compare($s) }
    elsif ($t eq 'if') { gen_if($s) }
    elsif ($t eq 'lap') { gen_lap($s) }
    elsif ($t eq 'put') { gen_put($s) }
    elsif ($t eq 'way_stmt') { gen_way_stmt($s) }
    elsif ($t eq 'due') { gen_due($s) }
    elsif ($t eq 'aliasput') { gen_aliasput($s) }
    elsif ($t eq 'tie') { emitc($tie_store[$s->{idx}]) }
    elsif ($t eq 'nop') { }
}

sub stage1_template {
    return <<'ASM';
[BITS 16]
[ORG 0x7C00]
_entry:
cli
xor ax, ax
mov ds, ax
mov es, ax
mov ss, ax
mov sp, 0x7C00
sti
mov [S_bootdrv], dl
mov ah, 0x02
mov al, 64
mov ch, 0
mov cl, 2
mov dh, 0
mov dl, [S_bootdrv]
mov bx, 0x7E00
int 0x13
jmp stage2
S_bootdrv: db 0
times 510-($-$$) db 0
dw 0xAA55
stage2:
ASM
}

sub gen_program {
    my ($ast) = @_;
    @code_lines = ();
    @data_defs = ();
    %symtab = ();
    $gid = 0;
    $disk_sector_counter = 100;
    gen_block($ast->{body});
    my $header = stage1_template();
    my $code = join("\n", @code_lines);
    my $halt = "cli\nhlt\njmp \$";
    my $data = join("\n", @data_defs);
    return "$header\n$code\n$halt\n$data\n";
}

my $infile = shift @ARGV;
my $outfile = shift @ARGV;

eval {
    open(my $fh, '<', $infile) or die;
    local $/;
    my $raw = <$fh>;
    close $fh;
    my ($src1, $ties) = extract_ties($raw);
    @tie_store = @$ties;
    my $src2 = strip_comments($src1);
    @TOKS = @{ tokenize($src2) };
    $POS = 0;
    my $ast = parse_program();
    my $asm = gen_program($ast);
    open(my $out, '>', $outfile) or die;
    print $out $asm;
    close $out;
    1;
};
