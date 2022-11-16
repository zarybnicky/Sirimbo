import * as React from 'react';
import { useCohortListQuery, useDeleteUserMutation, useRoleListQuery, useUserListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';
import { format } from 'date-fns';
import { Button } from 'components/Button';

export default function UserectoryList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useUserListQuery({ limit, offset: page * limit });
  const { mutateAsync: doDelete } = useDeleteUserMutation({
    onSuccess: () => refetch(),
  });

  const { data: roles } = useRoleListQuery();
  const { data: cohorts } = useCohortListQuery();

  const rowCount = data?.users?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/users/add">Přidat uživatele</Button>
    <Button href="/admin/users/duplicate">Duplicitní uživatelé</Button>
    <Button href="/admin/users/unconfirmed">Nepotvrzení uživatelé</Button>
    <Button href="/admin/users/statistiky">Statistiky</Button>
    <Button href="/admin/users/getMsmtCsv">MŠMT export</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      getRowId={row => row.uId}
      rows={data?.users?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/users/edit/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="sign-as"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/users/sign-as/${id}`)}
              label="Přihlásit se jako..."
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat uživatele" />,
          ], flex: 1,
        },
        {
          field: 'uPrijmeni', headerName: 'Jméno', flex: 1,
          valueGetter: ({ row }) => `${row.uJmeno} ${row.uPrijmeni}`,
        },
        {
          field: 'uNarozeni', headerName: 'Jméno', flex: 1,
          valueFormatter: ({ value }) => format(new Date(value), 'd. M. y'),
        },
        {
          field: 'uGroup', headerName: 'Role', flex: 1,
          renderCell: ({ value }) => roles?.permissions?.nodes.find(x => x.peId === value)?.peName,
        },
        {
          field: 'uSkupina', headerName: 'Skupina', flex: 1,
          renderCell: ({ value }) => {
            const cohort = cohorts?.skupinies?.nodes.find(x => x.sId === value);
            if (!cohort) return null;
            return <>
              <div className="w-3.5 h-3.5" title={cohort.sName} style={{ backgroundColor: cohort.sColorRgb }} />
              {' '}{cohort.sName}
            </>
          }
        },
        {
          field: 'uId', headerName: 'Var. symbol', flex: 1,
          valueFormatter: ({ value }) => (value as string).padStart(6, '0'),
        },

      ]}
    />
  </div>;
}
