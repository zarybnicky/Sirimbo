import { useRouter } from 'next/router';
import { NextLinkComposed } from "components/Link";
import { useCohortListQuery, useDeleteCohortMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';

export default function CohortsPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useCohortListQuery();
  const { mutateAsync: doDelete } = useDeleteCohortMutation({
    onSuccess: () => refetch(),
  });

  return <>
    <NextLinkComposed href="/admin/skupiny/add" className="btn btn-primary">Nová skupina</NextLinkComposed>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.sId}
      rows={data?.skupinies?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/skupiny/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton
              key="delete" title="smazat skupinu"
              params={{ id: id.toString() }} onDelete={doDelete}
            />,
          ]
        },
        {
          field: 'sName', headerName: 'Jméno', flex: 1,
          renderCell: ({ row }) => <>
            <div className="box" title={row.sDescription} style={{ backgroundColor: row.sColorRgb }} />
            {row.sName}{row.sLocation && `, ${row.sLocation}`}
            {!row.sVisible && ` (skrytá)`}
          </>,
        },
      ]}
    />
  </>;
}
