import { useRouter } from 'next/router';
import { usePaymentGroupListQuery, useDeletePaymentGroupMutation } from "lib/graphql/Payment";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PlatbyGroupListPage() {
  const router = useRouter();
  const { data, refetch } = usePaymentGroupListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentGroupMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/platby/structure/group/add">
      Nová skupina plateb
    </Button>

    <DataGrid
      autoHeight={true}
      rows={data?.platbyGroups?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/structure/group/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat skupinu plateb" />,
          ]
        },
        { field: 'pgName', headerName: 'Jméno', flex: 1 },
        {
          field: 'pgType', headerName: 'Typ', flex: 1,
          renderCell: ({ row }) => row.pgType ? 'Členské příspěvky' : 'Běžné platby',
        },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby, PermissionLevel.P_OWNED,
);
