import * as React from 'react';
import { Checkbox } from 'components/Checkbox';
import { useEventListQuery, useToggleEventVisibleMutation, useDeleteEventMutation } from 'lib/graphql/Event';
import { useRouter } from 'next/router';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';
import { formatLongDateRange } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function AdminEventList() {
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useEventListQuery({ limit, offset: page * limit });
  const { mutate: toggleVisible } = useToggleEventVisibleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doDelete } = useDeleteEventMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.akces?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/akce/add">Přidat</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      rows={data?.akces?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/akce/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="edit-detail"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/akce/detail/${id}`)}
              label="Upravit účastníky"
            />,
            <GridActionsCellItem key="edit-detail"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/akce/dokumenty/${id}`)}
              label="Upravit dokumenty"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat akci" />,
          ], flex: 1
        },
        { field: 'aJmeno', headerName: 'Jméno', flex: 1 },
        {
          field: 'aOd', headerName: 'Datum', flex: 1,
          renderCell: ({ row }) => formatLongDateRange(new Date(row.aOd), new Date(row.aDo)),
        },
        {
          field: 'aKapacita', headerName: 'Kapacita', flex: 1,
          renderCell: ({ row }) => <>{row.akceItemsByAiIdRodic.totalCount || 0}/{row.aKapacita}</>
        },
        {
          field: 'aVisible', headerName: 'Viditelná', flex: 1,
          renderCell: ({ row }) => (
            <Checkbox checked={row.aVisible} onChange={() => toggleVisible({ id: row.id, visible: !row.aVisible })} />
          ),
        },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce, PermissionLevel.P_OWNED,
);
