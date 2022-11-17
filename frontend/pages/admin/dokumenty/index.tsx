import * as React from 'react';
import { useDeleteFileMutation, useFileListQuery } from 'lib/graphql/Documents';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';
import { format } from 'date-fns';
import { Button } from 'components/Button';

const categories = [
  { id: 1, label: "Schůze,\u{00A0}rady" },
  { id: 2, label: 'Soutěže' },
  { id: 3, label: 'Soustředění' },
  { id: 0, label: 'Ostatní' },
];

export default function FileAdminList() {
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useFileListQuery({ limit, offset: page * limit });
  const { mutateAsync: doDelete } = useDeleteFileMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.dokumenties?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  // $fileUpload = $_FILES['file']['tmp_name'];
  // $fileName = str_replace(
  //     ['#', '$', '%', '&', '^', '*', '?'],
  //     ['No.', 'Dolar', 'Procento', 'And', ''],
  //     $_FILES['file']['name']
  // );
  // if (!$_POST['name']) {
  //     $_POST['name'] = $fileName;
  // }

  // $path = UPLOADS . '/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);
  // if (!move_uploaded_file($fileUpload, $path)) {
  //     \Message::danger('Soubor se nepodařilo nahrát.');
  //     \Redirect::to('/admin/dokumenty');
  // }

  // chmod($path, 0666);
  // \DBDokumenty::addDokument(
  //     $path,
  //     $_POST['name'],
  //     $fileName,
  //     $_POST['kategorie'],
  //     \Session::getUser()->getId()
  // );

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/dokumenty/add">Nový soubor</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      rows={data?.dokumenties?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/dokumenty/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat soubor" />,
          ],
        },
        {
          field: 'dName', headerName: 'Soubor', flex: 1,
          renderCell: ({ row }) => (
            <a target="_blank" rel="noreferrer" href={`/old/member/download?id=${row.id}`}>{row.dName}</a>
          ),
        },
        {
          field: 'category', headerName: 'Kategorie', width: 150,
          renderCell: ({ row }) => categories.find(x => x.id === row.dKategorie)?.label,
        },
        {
          field: 'date', headerName: 'Přidáno', flex: 1,
          renderCell: ({ row }) => row.dTimestamp ? format(new Date(row.dTimestamp), 'd. M. y') : '',
        }
      ]}
    />
  </div>;
}
