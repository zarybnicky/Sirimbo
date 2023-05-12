import * as React from 'react';
import { useFileListQuery } from 'lib/graphql/Documents';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';

const categories = [
  { id: 1, label: 'Schůze,\u{00A0}rady' },
  { id: 2, label: 'Soutěže' },
  { id: 3, label: 'Soustředění' },
  { id: 0, label: 'Ostatní' },
];

export default function FileAdminList() {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? (id as string) : null;
  const [search, setSearch] = React.useState('');
  const { data, refetch } = useFileListQuery();

  // $fileUpload = $_FILES['file']['tmp_name'];
  // $fileName = str_replace(
  //     ['#', '$', '%', '&', '^', '*', '?'],
  //     ['No.', 'Dolar', 'Procento', 'And', ''],
  //     $_FILES['file']['name']
  // ) ?? $fileName;
  // $path = UPLOADS . '/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);
  // if (!move_uploaded_file($fileUpload, $path)) {
  //     \Message::danger('Soubor se nepodařilo nahrát.');
  //     \Redirect::to('/admin/dokumenty');
  // }
  // chmod($path, 0666);
  // \DBDokumenty::addDokument($path, $_POST['name'], $fileName, $_POST['kategorie']);

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
          {/* <Button href="/admin/dokumenty/add">Nový soubor</Button> */}

      <FuzzyList
        data={data?.dokumenties?.nodes || []}
        fields={['id', 'dName']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={active === item.id}
            href={{pathname: '/admin/dokumenty/[id]', query: {id: item.id}}}
            title={
              <a target="_blank" rel="noreferrer" href={`/member/download?id=${item.id}`}>
                {item.dName}
              </a>
            }
            subtitle={
              item.dTimestamp ? fullDateFormatter.format(new Date(item.dTimestamp)) : ''
            }
          />
        )}
      />
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peDokumenty,
  PermissionLevel.P_OWNED,
);
