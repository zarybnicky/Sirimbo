import * as React from 'react';
import { useDeleteUserMutation, useUserListQuery } from 'lib/graphql/User';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { useRoleListQuery } from 'lib/graphql/Roles';
import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { formatFullDate } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function Userist() {
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
      rows={data?.users?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/users/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="sign-as"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/users/sign-as/${id}`)}
              label="Přihlásit se jako..."
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat uživatele" />,
          ], flex: 1,
        },
        {
          field: 'uPrijmeni', headerName: 'Jméno', flex: 1,
          valueGetter: ({ row }) => `${row.uJmeno} ${row.uPrijmeni}`,
        },
        {
          field: 'uNarozeni', headerName: 'Datum narození', flex: 1,
          valueFormatter: ({ value }) => formatFullDate(new Date(value)),
        },
        {
          field: 'uGroup', headerName: 'Role', flex: 1,
          renderCell: ({ value }) => roles?.permissions?.nodes.find(x => x.id === value)?.peName,
        },
        {
          field: 'uSkupina', headerName: 'Skupina', flex: 1,
          renderCell: ({ value }) => {
            const cohort = cohorts?.skupinies?.nodes.find(x => x.id === value);
            if (!cohort) return null;
            return <>
              <div className="w-3.5 h-3.5" title={cohort.sName} style={{ backgroundColor: cohort.sColorRgb }} />
              {' '}{cohort.sName}
            </>
          }
        },
        {
          field: 'id', headerName: 'Var. symbol', flex: 1,
          valueFormatter: ({ value }) => (value as string).padStart(6, '0'),
        },

      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers, PermissionLevel.P_OWNED,
);
