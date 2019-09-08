<?php
return [
    ['Nástěnka', '/member/home', []],
    ['Rozpis', '/member/rozpis', []],
    ['Nabídka', '/member/nabidka', []],
    ['Akce', '/member/akce', []],
    ['Dokumenty', '/member/dokumenty', []],
    ['Členové', '/member/clenove/structure', []],
    ['Profil', '/member/profil', []],
    ['Administrace', '/admin', include SETTINGS . '/menu/admin.inner.php', ['nastenka', P_OWNED]]
];
