import { useRouter } from 'next/router';
import { NextLinkComposed } from "components/Link";
import { usePaymentCategoryListQuery, useDeletePaymentCategoryMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';
import format from 'date-fns/format';

export default function PlatbyCategoryListPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = usePaymentCategoryListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentCategoryMutation({
    onSuccess: () => refetch(),
  });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/platby/structure/category/add" className="btn btn-primary">
      Nová platba
    </NextLinkComposed>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.pcId}
      rows={data?.platbyCategories?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/structure/category/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat platbu" />,
          ]
        },
        { field: 'pcName', headerName: 'Název', flex: 1 },
        { field: 'pcSymbol', headerName: 'Specifický symbol', flex: 1 },
        {
          field: 'pcValidFrom', headerName: 'Platnost', flex: 1,
          renderCell: ({ row }) => `${format(new Date(row.pcValidFrom), 'd. M. y')} - ${format(new Date(row.pcValidTo), 'd. M. y')}`,
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
  </Container>;
}