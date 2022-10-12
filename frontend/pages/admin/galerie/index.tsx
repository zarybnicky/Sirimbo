import * as React from 'react';
import { Pagination, Checkbox, Button, Menu, MenuItem } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { NextLinkComposed } from 'components/Link';
import { useGalleryDirListQuery, useToggleGalleryDirVisibleMutation } from 'index';

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
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useGalleryDirListQuery({
    limit, offset: (page - 1) * limit,
  });
  const { mutateAsync: toggleVisible } = useToggleGalleryDirVisibleMutation({
    onSuccess: () => refetch(),
  });

  const roots = listToTree((data?.galerieDirs?.nodes || []).map(x => ({
    ...x,
    id: x.gdId,
    parentId: x.gdIdRodic,
    children: [],
  })));
  const dataSorted = roots.length > 0 ? flatten(roots[0]!) : [];
  const total = data?.galerieDirs?.totalCount || 0;

  const list = !total ? null : <table>
    <thead><tr><th>Složka</th><th>Skrytá</th></tr></thead>
    <tbody>
      {dataSorted.map((a) => <tr key={a.gdId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>{'→'.repeat(a.gdLevel - 1)} {a.gdName}</Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/galerie/directory/edit/${a.gdId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/galerie/directory/${a.gdId}`}>
                  Upravit fotky
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/galerie/directory/remove/${a.gdId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
        </td>
        <td>
          <Checkbox checked={a.gdHidden} onChange={() => toggleVisible({
            id: a.gdId, visible: !a.gdHidden,
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <>
    <NextLinkComposed href="/admin/galerie/file/upload" className="btn btn-outline-primary">Přidat fotky</NextLinkComposed>
    <NextLinkComposed href="/admin/galerie/directory/add" className="btn btn-outline-primary">Přidat složku</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}
