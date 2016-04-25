-type var_type() :: (
        % standard derived types for std::atomic
        char |
        schar |
        uchar |
        short |
        ushort |
        int |
        uint |
        long |
        ulong |
        llong |
        ullong |
        char16 |
        char32 |
        wchar |
        int_least8 |
        uint_least8 |
        int_least16 |
        uint_least16 |
        int_least32 |
        uint_least32 |
        int_least64 |
        uint_least64 |
        int_fast8 |
        uint_fast8 |
        int_fast16 |
        uint_fast16 |
        int_fast32 |
        uint_fast32 |
        int_fast64 |
        uint_fast64 |
        intmax |
        uintmax |
        % useful extras
        int8 |
        uint8 |
        int16 |
        uint16 |
        int32 |
        uint32 |
        int64 |
        uint64).

-type memory_order() :: (memory_order_relaxed |
                         memory_order_consume |
                         memory_order_acquire |
                         memory_order_release |
                         memory_order_acq_rel |
                         memory_order_seq_cst).
