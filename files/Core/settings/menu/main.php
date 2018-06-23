<?php
return [
    ['O klubu', '/oklubu', require SETTINGS . '/menu/oklubu.php'],
    ['Novinky', '/aktualne/clanky'],
    ['Videa', '/video'],
    ['Fotogalerie', '/fotogalerie'],
    ['Kontakt', '/kontakt'],
    'w-100',
    ['Pro členy', '/member', include SETTINGS . '/menu/member.inner.php', ['nastenka', P_VIEW]],
    ['Administrace', '/admin', include SETTINGS . '/menu/admin.inner.php', ['nastenka', P_OWNED]]
];
