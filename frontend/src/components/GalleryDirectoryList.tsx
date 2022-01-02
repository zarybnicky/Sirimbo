import * as React from 'react';
import { Link } from 'react-router-dom';
import { Checkbox, Button, Menu, MenuItem } from '@material-ui/core';
import { Pagination } from '@material-ui/lab';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { $, GalerieDirsOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';

const GalleryDirList = Selector('Query')({
  allGalerieDirs: [
    { first: $`limit`, offset: $`offset`, orderBy: [GalerieDirsOrderBy.GD_NAME_ASC] },
    {
      nodes: {
        gdHidden: true,
        gdId: true,
        gdIdRodic: true,
        gdLevel: true,
        gdName: true,
        gdPath: true,
      },
      totalCount: true,
    }
  ],
});

const ToggleVisible = Selector('Mutation')({
  updateGalerieDirByGdId: [
    { input: { gdId: $`id`, galerieDirPatch: { gdHidden: $`visible` } } },
    {
      galerieDir: {
        gdId: true,
      },
    },
  ],
});

type Treeified<T> = T & { id: number; parentId: number; children: Treeified<T>[]; };
function listToTree<T>(list: Treeified<T>[]) {
  const map: { [k: number]: number } = {};
  const roots = []
  for (let i = 0; i < list.length; i += 1) {
    map[list[i].id] = i;
  }
  for (let i = 0; i < list.length; i += 1) {
    if (list[i].parentId === list[i].id) {
      roots.push(list[i]);
    } else {
      list[map[list[i].parentId]].children.push(list[i]);
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

export function GalleryDirectoryList() {
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useTypedQuery(GalleryDirList, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const roots = listToTree((data?.allGalerieDirs?.nodes || []).map(x => ({
    ...x,
    id: x.gdId,
    parentId: x.gdIdRodic,
    children: [],
  })));
  const dataSorted = roots.length > 0 ? flatten(roots[0]) : [];
  const [toggleVisible] = useTypedMutation(ToggleVisible, {
    onCompleted: () => refetch(),
  });
  const total = data?.allGalerieDirs?.totalCount || 0;

  const list = !total ? null : <table>
    <thead><tr><th>Složka</th><th>Skrytá</th></tr></thead>
    <tbody>
      {dataSorted.map((a) => <tr key={a.gdId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
              <Button {...bindTrigger(popupState)}>{'→'.repeat(a.gdLevel - 1)} {a.gdName}</Button>
              <Menu {...bindMenu(popupState)} getContentAnchorEl={null}>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/galerie/directory/edit/${a.gdId}`}>
                  Upravit
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/galerie/directory/${a.gdId}`}>
                  Upravit fotky
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/galerie/directory/remove/${a.gdId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        </td>
        <td>
          <Checkbox checked={a.gdHidden} onChange={() => toggleVisible({
            variables: { id: a.gdId, visible: !a.gdHidden },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <React.Fragment>
    <a className="btn btn-outline-primary" href="/admin/galerie/file/upload">Přidat fotky</a>
    <a className="btn btn-outline-primary" href="/admin/galerie/directory/add">Přidat složku</a>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}
