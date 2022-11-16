import * as React from 'react';
import { Checkbox } from 'components/Checkbox';
import { Button } from 'components/Button';
import { useDeleteGalleryDirMutation, useGalleryDirListQuery, useToggleGalleryDirVisibleMutation } from 'lib/graphql/Gallery';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';

type Treeified<T> = T & { id: string; parentId: string; children: Treeified<T>[]; };
function listToTree<T>(list: Treeified<T>[]) {
  const map: { [k: string]: number } = {};
  const roots = []
  for (let i = 0; i < list.length; i += 1) {
    map[list[i]!.id] = i;
  }
  for (let i = 0; i < list.length; i += 1) {
    if (list[i]!.parentId === list[i]!.id) {
      roots.push(list[i]);
    } else {
      list[map[list[i]!.parentId]!]!.children.push(list[i]!);
    }
  }
  return roots;
}
function flatten<T>(root: Treeified<T>): T[] {
  const output: T[] = [];
  const stack = [root];
  while (stack.length > 0) {
    let node = stack.pop()!!;
    output.push(node);
    node.children.forEach((x) => stack.push(x))
  }
  return output;
}

export default function GalleryDirectoryList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useGalleryDirListQuery({
    limit, offset: page * limit,
  });
  const { mutateAsync: toggleVisible } = useToggleGalleryDirVisibleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doDelete } = useDeleteGalleryDirMutation({
    onSuccess: () => refetch(),
  });

  const roots = listToTree((data?.galerieDirs?.nodes || []).map(x => ({
    ...x,
    id: x.gdId,
    parentId: x.gdIdRodic,
    children: [],
  })));
  const dataSorted = roots.length > 0 ? flatten(roots[0]!) : [];

  const rowCount = data?.galerieDirs?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/galerie/file/upload">Přidat fotky</Button>
    <Button href="/admin/galerie/directory/add">Přidat složku</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      getRowId={row => row.gdId}
      rows={dataSorted}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/galerie/directory/edit/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/galerie/directory/${id}`)}
              label="Upravit fotky"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat složku" />,
          ], flex: 1,
        },
        {
          field: 'gdName', headerName: 'Jméno', flex: 1,
          valueGetter: ({ row }) => `${'→'.repeat(row.gdLevel - 1)} ${row.gdName}`,
        },
        {
          field: 'gdHidden', headerName: 'Skrytá', flex: 1,
          renderCell: ({ row }) => <>
            <Checkbox checked={row.gdHidden} onChange={() => toggleVisible({
              id: row.gdId, visible: !row.gdHidden,
            })} />
          </>,
        },
      ]}
    />
  </div>;
}
