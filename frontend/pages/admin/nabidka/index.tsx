import * as React from 'react';
import { Checkbox } from 'components/Checkbox';
import { DateRange } from 'components/DateRange';
import { useDeleteReservationMutation, useReservationListQuery, useToggleReservationVisibleMutation } from 'lib/graphql';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon, Copy as ContentCopyIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';

export default function ReservationAdminList() {
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useReservationListQuery({ limit, offset: page * limit });
  const { mutateAsync: toggleVisible } = useToggleReservationVisibleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doDelete } = useDeleteReservationMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.nabidkas?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/nabidka/add">Nová nabídka</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      getRowId={row => row.nId}
      rows={data?.nabidkas?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/nabidka/edit/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="edit-lessons"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/nabidka/detail/${id}`)}
              label="Upravit lekce"
            />,
            <GridActionsCellItem key="duplicate"
              icon={<ContentCopyIcon />}
              onClick={() => router.push(`/admin/nabidka/duplicate/${id}`)}
              label="Duplikovat"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat nabídku" />,
          ], flex: 1,
        },
        {
          field: 'userByNTrener', headerName: 'Trenér', flex: 1,
          renderCell: ({ row }) => `${row.userByNTrener?.uJmeno} ${row.userByNTrener?.uPrijmeni}`,
        },
        {
          field: 'nOd', headerName: 'Datum', flex: 1,
          renderCell: ({ row }) => <DateRange from={row.nOd} to={row.nDo} />,
        },
        {
          field: 'nVisible', headerName: 'Viditelný', flex: 1,
          renderCell: ({ row }) => (
            <Checkbox checked={row.nVisible} onChange={() => toggleVisible({ id: row.nId, visible: !row.nVisible })} />
          ),
        },
      ]}
    />
  </div>;
}
