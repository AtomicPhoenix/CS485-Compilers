-- as a test case this just represents a large amount of the language constructs that exist
-- syntactically AND semantically correct but also syntactically stupid and poorly written code
    -- i.e. great for testing!
Class Main -- Main is where the program starts
 inherits IO { -- inheriting from IO allows us to read and write strings

    main() : Object { -- this method is invoked when the program starts
        let 
            m : BadMap, -- the sorted list input lines
            done : Bool <- false, -- are we done reading lines? 
            deq : VDeque <- (new VDeque).init(),
            ans : VDeque <- (new VDeque).init(),
            ops : HelperFuns <- new HelperFuns
        in {
            while not done loop {
                let task : String <- in_string(), prereq_task : String <- in_string() in {
                if  ops.or(task = "", prereq_task = "") then {-- if we are done reading lines
                 --then s will be "" -- 
                    done <- true;
                } else { 
                -- l <- l.insert(s) -- insertion sort it into our list
                    if isvoid m then {
                        m <- (new BadMap).init(prereq_task, (new Vertex).init(prereq_task, task, ""));
                        m <- m.insert(task, (new Vertex).init(task, "", prereq_task));
                    } else {
                        if m.contains(prereq_task) then
                            m.get(prereq_task).get_dependents().insert(task)
                        else
                            m <- m.insert(prereq_task, (new Vertex).init(prereq_task, task, ""))
                        fi;
                        if m.contains(task) then {
                            m.get(task).get_dependencies().insert(prereq_task);
                        } else
                            m <- m.insert(task, (new Vertex).init(task, "", prereq_task))
                        fi;
                    }
                    fi;

                } fi ;
                };
            } pool ; -- loop/pool deliniates a while loop body
            done <- false;
            let iter_map : BadMap <- m, m2 : BadMap <- m in {
            while not done loop {
                if iter_map.at_end() then
                    done <- true
                else
                    done <- false
                fi;
                if iter_map.get_value().get_dependencies().empty() then {
                    deq.push_back(iter_map.get_value());
                    let k : String <- iter_map.get_key() in {
                        iter_map <- iter_map.get_next();
                        m2 <- m2.remove(k);
                        if not m2 = m then
                            m <- m2
                        else
                            m <- m
                        fi;
                    };
                } else
                    iter_map <- iter_map.get_next()
                fi;
            } pool;
            };
            let immediate_break_out : Bool <- false in
            while ops.and(not (deq.empty()), not immediate_break_out) loop {
                let node : Vertex <- deq.pop_front(), void_node : SNode in {
                    ans.push_back(node);
                    done <- false;
                    let dep : SNode <- node.get_dependents().get_head() in {
                    -- hacky way to detect a cycle of size 1 (breaks otherwise :( )
                    -- I only got this because of the testcase I wrote so good for me I guess
                    if node.get_dependents().contains(node.get_task()) then {
                        done <- true;
                        dep <- void_node;
                        deq.push_back(node);
                        immediate_break_out <- true;
                    } else
                        done <- false
                    fi;

                    while ops.and(not isvoid dep, not node.get_dependents().empty()) loop {
                        m.get(dep.get_data()).get_dependencies().remove(node.get_task());
                        if m.get(dep.get_data()).get_dependencies().empty() then {
                            deq.push_front(m.get(dep.get_data()));
                            m <- m.remove(dep.get_data());
                            --m.print();
                        } else
                            done <- false
                        fi;
                        dep <- dep.get_next();
                    } pool;
                    };
                };
            } pool;
            if not isvoid m then {
                out_string("cycle\n");
            } else
                while not ans.empty() loop {
                    out_string(ans.pop_front().get_task());
                    out_string("\n");
                } pool
            fi;
        }
    };
};

Class Vertex inherits IO {
    task : String;
    dependents : SList;
    dependencies : SList;
    ops : HelperFuns <- new HelperFuns;
    init(ts : String, dependent : String, dependency : String) : Vertex {{
        task <- ts;
        if dependent = "" then
            dependents <- (new SList).init("")
        else
            dependents <- (new SList).init(dependent)
        fi;
        if dependency = "" then
            dependencies <- (new SList).init("")
        else
            dependencies <- (new SList).init(dependency)
        fi;
        self;
    }};
    print() : Object {{
        out_string(" dependents: ");
        dependents.print();
        out_string(" dependencies: ");
        dependencies.print();
        out_string("\n");
    }};
    get_dependents() : SList { dependents };
    get_dependencies() : SList { dependencies };
    get_task() : String { task };
};

Class SList inherits IO {
    head : SNode;
    tail : SNode;
    -- cheeky way to always have a void for assigning :)
    void_node : SNode;
    ops : HelperFuns <- new HelperFuns;
    init(data : String) : SList {{
        if not (data = "") then
            head <- (new SNode).init(data)
        else
            head
        fi;
        --tail <- head;
        self;
    }};
    contains(itm : String) : Bool {{
        let cur : SNode <- head,
            ret : Bool <- false in {
        while ops.and(not (isvoid cur),ret = false) loop {
            if cur.get_data() = itm then
                ret <- true
            else
                cur <- cur.get_next()
            fi;
        } pool ;
        ret;
        };
    }};
    insert(s : String) : Object {
        let done : Bool <- false, cur : SNode <- head in {
        while not done loop {
            if isvoid head then {
                -- empty list :(
                head <- (new SNode).init(s);
                done <- true;
            } else
                if isvoid cur then {
                    -- empty list, shouldn't be possible
                    done <- true;
                } else {
                    --if given str <= current str, go later and check next
                    if not ops.s_gt(s, cur.get_data()) then {
                        if s = cur.get_data() then
                            -- sike this is actually a set lmao; just ignore repeat elements
                            done <- true
                        else
                            if not isvoid cur.get_next() then
                                if not ops.s_gt(s, cur.get_next().get_data()) then
                                    -- all checks passed, go to next
                                    cur <- cur.get_next()
                                else {
                                    -- insert time!
                                    if not cur.get_next().get_data() = s then
                                        let nxt : SNode <- cur.get_next() in {
                                            cur.set_next((new SNode).init(s));
                                            cur.get_next().set_next(nxt);
                                            done <- true;
                                        }
                                    else
                                        done <- true
                                    fi;

                                }
                                fi
                            else {
                                -- tail
                                cur <- cur.set_next((new SNode).init(s));
                                tail <- cur.get_next();
                                done <- true;
                            } fi
                        fi;
                    } else {
                        -- unless I've failed my algorithms class this should only happen if the head of the list is here, in which case we gotta basically return a new head
                        -- we can refer to the new "next" as self because of this assumption
                        -- even if i did fail it whatever code works :)))))
                        cur <- (new SNode).init(s);
                        cur.set_next(head);
                        head <- cur;
                        done <- true;
                    } fi;
                } fi
            --} fi
            fi;
        } pool;
        }
    };
    remove(s : String) : Object {
        let done : Bool <- false, cur : SNode <- head in
        while not done loop {
            if isvoid cur then
                done <- true
            else
                if head = tail then
                    if cur.get_data() = s then {
                        head <- head.get_next();
                        tail <- void_node;
                        done <- true;
                    } else
                        cur <- cur.get_next()
                    fi
                else
                    if cur = head then
                        if cur.get_data() = s then {
                            head <- head.get_next();
                            tail <- void_node;
                            done <- true;
                        } else
                            if not isvoid cur.get_next() then
                                if cur.get_next().get_data() = s then {
                                    if cur.get_next() = tail then {
                                        tail <- head;
                                        head.set_next(void_node);
                                    }
                                    else
                                        cur.set_next(cur.get_next().get_next())
                                    fi;
                                    done <- true;
                                } else
                                    cur <- cur.get_next()
                                fi
                            else
                                done <- true
                            fi
                        fi
                        
                    else
                        if not isvoid cur.get_next() then
                            if cur.get_next().get_data() = s then {
                                if cur.get_next() = tail then
                                    tail <- cur
                                else
                                    cur.set_next(cur.get_next().get_next())
                                fi;
                                done <- true;
                            } else
                                cur <- cur.get_next()
                            fi
                        else
                            done <- true
                        fi
                    fi
                fi
            fi;
        } pool
    };
    empty() : Bool { isvoid head };
    print() : Object {
        let done : Bool <- false, cur : SNode <- head in {
            while not done loop {
                if isvoid cur then {
                    out_string("}");
                    done <- true;
                } else
                    if cur = tail then {
                        out_string(", ");
                        out_string(cur.get_data());
                        out_string("}");
                        done <- true;
                    } else
                        if cur = head then {
                            out_string("{");
                            out_string(cur.get_data());
                            cur <- cur.get_next();
                        } else {
                            out_string(", ");
                            out_string(cur.get_data());
                            cur <- cur.get_next();
                        } fi
                    fi
                fi;

            } pool;

        }
    };

            
    get_head() : SNode { head };
    get_tail() : SNode { tail };
} ;

Class SNode inherits IO {
    data : String;
    next : SNode;
    ops : HelperFuns <- new HelperFuns;
    init(d : String) : SNode {{
        data <- d;
        self;
    }};
    set_next(n : SNode) : SNode { next <- n };
    get_next() : SNode { next };
    get_data() : String { data };
    set_data(s : String) : String { data <- s };
};

Class BadMap inherits IO {
    key : String;
    value : Vertex;
    next : BadMap;
    init(k : String, v: Vertex) : BadMap {{
        key <- k;
        value <- v;
        self;
    }};
    get(k : String) : Vertex {{
        let done : Bool <- false, cur : BadMap <- self, ret : Vertex in {
            while not done loop {
                if isvoid cur then
                    done <- true
                else 
                    if k = cur.get_key() then {
                        done <- true;
                        ret <- cur.get_value();
                    } else
                        cur <- cur.get_next()
                    fi
                fi;
            } pool;
            ret;
        };
    }};
    set(k : String, v : Vertex) : Object {{
        let done : Bool <- false, cur : BadMap <- self in {
            while not done loop {
                if isvoid cur then
                    done <- true
                else {
                    if k = cur.get_key() then {
                        done <- true;
                        cur.set_value(v);
                    } else
                        -- cur <- cur.get_next()
                        if isvoid cur.get_next() then {
                            -- make maps behave like c++ kinda
                            insert(k, v);
                            done <- true;
                        } else
                            cur <- cur.get_next()
                        fi
                    fi;
                } fi;
            } pool;
        };
    }};
    -- inserts, and returns the new head of the map (which is really just a linked list with extra steps and a fun interface)
    insert(k: String, v : Vertex) : BadMap {
        let done : Bool <- false, cur : BadMap <- self in {
            while not done loop {
                if isvoid cur then {
                    -- empty map, shouldn't be possible
                    done <- true;
                } else {
                    --if given key >= current key, go later and check next
                    if not (k < cur.get_key()) then {
                        if k = cur.get_key() then {
                            cur <- self;
                            done <- true;
                        } else
                            if not isvoid cur.get_next() then
                                if not (k < cur.get_next().get_key()) then
                                    -- all checks passed, go to next
                                    cur <- cur.get_next()
                                else {
                                    -- insert time!
                                    let nxt : BadMap <- cur.get_next() in {
                                        cur.set_next((new BadMap).init(k,v));
                                        cur.get_next().set_next(nxt);
                                        done <- true;
                                        cur <- self;
                                    };

                                }
                                fi
                            else {
                                -- tail
                                cur.set_next((new BadMap).init(k,v));
                                cur <- self;
                                done <- true;
                            } fi
                        fi;
                    } else {
                        -- unless I've failed my algorithms class this should only happen if the head of the list is here, in which case we gotta basically return a new head
                        -- we can refer to the new "next" as self because of this assumption
                        -- even if the assumption is wrong it works /shrug
                        cur <- (new BadMap).init(k, v);
                        cur.set_next(self);
                        done <- true;
                    } fi;
                } fi;
            } pool ;
            cur;
        }
    };
    contains(k : String) : Bool {
        let done : Bool<- false, cur : BadMap <- self, ret : Bool <- false in {
        while not done loop {
            if cur.get_key() = k then {
                ret <- true;
                done <- true;
            } else
                if isvoid cur.get_next() then
                    done <- true
                else
                    cur <- cur.get_next()
                fi
            fi;

        } pool;
        ret;
        }
    };
    remove(s : String) : BadMap {
        let done : Bool <- false, cur : BadMap <- self in {
        while not done loop {
            if isvoid cur then
                done <- true
            else
                if cur = self then
                    if cur.get_key() = s then {
                        cur <- get_next();
                        done <- true;
                    } else
                        if not isvoid cur.get_next() then
                            if cur.get_next().get_key() = s then {
                                cur.set_next(cur.get_next().get_next());
                                cur <- self;
                                done <- true;
                            } else
                                cur <- cur.get_next()
                            fi
                        else {
                            cur <- cur.get_next();
                        } fi
                    fi
                else
                    if not isvoid cur.get_next() then
                        if cur.get_next().get_key() = s then {
                            cur.set_next(cur.get_next().get_next());
                            cur <- self;
                            done <- true;
                        } else
                            cur <- cur.get_next()
                        fi
                    else {
                        cur <- self;
                        done <- true;
                    } fi
                fi
            fi;
        } pool;
        cur;
        }
    };
    print() : Object {
        let done : Bool <- false, cur : BadMap <- self in {
            while not done loop {
                if not isvoid cur then {
                    out_string(cur.get_key());
                    out_string(": {");
                    cur.get_value().print();
                    cur <- cur.get_next();
                } else
                    done <- true
                fi;

            } pool;
        }
    };
    at_end() : Bool { isvoid next };
    get_key() : String { key };
    get_value() : Vertex { value };
    get_next() : BadMap { next };
    set_value(v : Vertex) : Vertex { value <- v };
    set_next(m : BadMap) : BadMap { next <- m };
};

Class HelperFuns inherits Object {
    and(first : Bool, second : Bool) : Bool {
        if first then
            if second then
                true
            else
                false
            fi
        else
            false
        fi
    };
    or(first : Bool, second : Bool) : Bool {
        if first then
            true
        else
        if second then
            true
        else
            false
        fi
        fi
    };
    s_gt(first : String, second : String) : Bool {
        if and(not first = second, not first < second) then
            true
        else
            false
        fi
    };
};

Class VDeque inherits Object {
    head : VNode;
    tail : VNode;
    -- cheeky way to always have a void for assigning :)
    void_node : VNode;
    init() : VDeque {{
        self;
    }};
    push_front(v : Vertex) : Object {
        if isvoid head then {
            -- 0-elem
            head <- (new VNode).init(v);
            tail <- head;
        } else {
            if head = tail then {
                -- 1-elem
                let hed : VNode <- head in {
                head <- (new VNode).init(v);
                head.set_next(tail);
                };
            } else {
                let hd : VNode <- head in {
                    head <- (new VNode).init(v);
                    head.set_next(hd);
                };
            } fi;
        } fi
    };
    push_back(v : Vertex) : Object {
        if isvoid head then {
            -- 0-elem
            head <- (new VNode).init(v);
            tail <- head;
        } else {
            if head = tail then {
                -- 1-elem
                head.set_next((new VNode).init(v));
                tail <- head.get_next();
            } else {
                tail.set_next((new VNode).init(v));
                tail <- tail.get_next();
            } fi;
        } fi
    };
    pop_front() : Vertex {
        let v : Vertex <- head.get_data() in {
            if head = tail then {
                head <- void_node;
                tail <- void_node;
            } else
                head <- head.get_next()
            fi;
            v;
        }
    };
    pop_back() : Vertex {
        -- should've made this a doubly linked list specifically for THIS but whatever man, don't wanna reimplement everything
        let v : Vertex <- tail.get_data(), done : Bool <- false, cur : VNode <- head in {
            if head = tail then {
                head = void_node;
                tail = void_node;
            } else
                while not done loop {
                    if cur.get_next() = tail then {
                        tail <- cur;
                        done <- true;
                    } else
                        cur <- cur.get_next()
                    fi;
                } pool
            fi;
            v;
        }
    };
    empty() : Bool { isvoid head };

            
    get_head() : VNode { head };
    get_tail() : VNode { tail };

};
Class VNode inherits IO {
    data : Vertex;
    next : VNode;
    init(d : Vertex) : VNode {{
        data <- d;
        self;
    }};
    set_next(n : VNode) : VNode { next <- n };
    get_next() : VNode { next };
    get_data() : Vertex { data };
    set_data(v : Vertex) : Vertex { data <- v };
};
