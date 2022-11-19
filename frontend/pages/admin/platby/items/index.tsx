import { useRouter } from 'next/router';
import { usePaymentItemListQuery, useDeletePaymentItemMutation } from "lib/graphql/Payment";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';
import { formatFullDate } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PlatbyItemListPage() {
  const router = useRouter();
  const { data, refetch } = usePaymentItemListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentItemMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/platby/items/add">Nová platba</Button>

    <DataGrid
      autoHeight={true}
      rows={data?.platbyItems?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/items/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat platbu" />,
          ]
        },
        {
          field: 'piAmount', headerName: 'Částka', flex: 1,
          valueFormatter: ({ value }) => `${value} Kč`,
        },
        {
          field: 'piDate', headerName: 'Datum', flex: 1,
          valueFormatter: ({ value }) => formatFullDate(new Date(value)),
        },
        {
          field: 'piIdUser', headerName: 'Uživatel', flex: 1,
          valueGetter: ({ row }) => `${row.userByPiIdUser?.uJmeno} ${row.userByPiIdUser?.uPrijmeni}`,
        },
        {
          field: 'piIdCategory', headerName: 'Kategorie', flex: 1,
          valueGetter: ({ row }) => `${row.platbyCategoryByPiIdCategory?.pcName}`,
        },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby, PermissionLevel.P_OWNED,
);
