import { useRouter } from 'next/router';
import { usePaymentCategoryListQuery, useDeletePaymentCategoryMutation } from "lib/graphql/Payment";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';
import { formatLongDateRange } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PlatbyCategoryListPage() {
  const router = useRouter();
  const { data, refetch } = usePaymentCategoryListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentCategoryMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/platby/structure/category/add">Nová platba</Button>

    <DataGrid
      autoHeight={true}
      rows={data?.platbyCategories?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/structure/category/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat platbu" />,
          ]
        },
        { field: 'pcName', headerName: 'Název', flex: 1 },
        { field: 'pcSymbol', headerName: 'Specifický symbol', flex: 1 },
        {
          field: 'pcValidFrom', headerName: 'Platnost', flex: 1,
          renderCell: ({ row }) => formatLongDateRange(new Date(row.pcValidFrom), new Date(row.pcValidTo)),
        },
        {
          field: 'pcAmount', headerName: 'Částka', flex: 1,
          renderCell: ({ row }) => `${row.pcAmount}${row.pcUseBase ? ' * ?' : ''}`,
        },
        {
          field: 'pcArchive', headerName: 'Archiv', flex: 1,
          renderCell: ({ value }) => value ? 'ano' : '',
        },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby, PermissionLevel.P_OWNED,
);
