import * as React from 'react';
import type { Value } from '@react-page/editor';
import { ReactPage } from '../components/ReactPage';
import { CircularProgress, Grid, TextField, List, ListItem, ListItemText, Typography, Button, ListItemIcon } from '@material-ui/core';
import { useTypedLazyQuery, useTypedMutation, useTypedQuery } from '../zeus/apollo';
import { $, PagesOrderBy, GraphQLTypes } from '../zeus';
import AddIcon from '@material-ui/icons/Add';


type Page = GraphQLTypes['Page'];
type PageRevision = GraphQLTypes['PageRevision'];

type State = {
  state: 'empty';
  content?: Value;
} | {
  state: 'create';
  url: string;
  content?: Value;
} | {
  state: 'edit';
  page: Page;
  content?: Value;
} | {
  state: 'history';
  page: Page;
  revs: PageRevision[];
  current?: PageRevision;
  content?: Value;
};

export const EditorPage = ({ }) => {
  const { data, refetch } = useTypedQuery({
    allPages: [{ orderBy: [PagesOrderBy.URL_ASC] }, {
      nodes: {
        __typename: true,
        nodeId: true,
        id: true,
        url: true,
        content: true,
        createdAt: true,
        updatedAt: true,
      },
    }],
  });
  const [fetchRevs] = useTypedLazyQuery({
    allPageRevisions: [
      { condition: { id: $`id` } },
      {
        nodes: {
          __typename: true,
          nodeId: true,
          revNumber: true,
          revOperation: true,
          revTimestamp: true,
          id: true,
          url: true,
          content: true,
          createdAt: true,
          updatedAt: true,
        },
      },
    ],
  });
  const [doCreatePage] = useTypedMutation({
    createPage: [
      { input: { page: { url: $`url`, content: $`content` } } },
      {
        page: {
          __typename: true,
          nodeId: true,
          id: true,
          url: true,
          content: true,
          createdAt: true,
          updatedAt: true,
        }
      },
    ],
  });
  const [doSavePage] = useTypedMutation({
    updatePageById: [
      { input: { id: $`id`, pagePatch: { url: $`url`, content: $`content` } } },
      { __typename: true },
    ],
  }, {
    onCompleted: () => refetch(),
  });

  const [loading, setLoading] = React.useState<boolean>(false);
  const [state, setState] = React.useState<State>({ state: 'empty' });
  const startPage = () => setState({ state: 'create', url: '' });
  const selectPage = (page: Page) => setState({ state: 'edit', page, content: page.content })
  const setContent = (content: Value) => setState(s => ({ ...s, content }));

  let toolbar: JSX.Element | null = null;
  switch (state.state) {
    case 'create':
      const createPage = async () => {
        setLoading(true);
        const { data } = await doCreatePage({ variables: { url: state.url, content: state.content } });
        await refetch();
        setLoading(false);
        setState({
          state: 'edit', page: data?.createPage?.page, content: state.content
        } as State);
      };
      toolbar = <Grid container direction="column" spacing={2} style={{ marginBottom: '1rem' }}>
        <Grid item><Typography variant="h4">Nová stránka</Typography></Grid>
        <Grid item>
          <TextField value={state.url} placeholder="URL stránky" onChange={(e) => setState({
            ...state, url: e.target.value,
          })} />
        </Grid>
        <Grid item>
          <Button disabled={loading} variant="contained" onClick={createPage}>
            Vytvořit a publikovat
            {loading ? <CircularProgress size={20} /> : null}
          </Button>
        </Grid>
      </Grid>;
      break;

    case 'edit':
      const selectHistory = async () => {
        setState({ state: 'history', page: state.page, revs: [] });
        const { data } = await fetchRevs({ variables: { id: state.page.id } });
        setState(state => ({ ...state, revs: data?.allPageRevisions?.nodes || [] }));
      };
      const savePage = async () => {
        setLoading(true);
        const { id, url } = state.page;
        await doSavePage({ variables: { id, url, content: state.content } });
        await refetch();
        setLoading(false);
      };
      toolbar = <Grid container direction="column" spacing={2} style={{ marginBottom: '1rem' }}>
        <Grid item><Typography variant="h4">Upravit stránku</Typography></Grid>
        <Grid item>
          <TextField value={state.page.url} placeholder="URL stránky" onChange={(e) => setState({
            ...state,
            page: { ...state.page, url: e.target.value },
          })} />
        </Grid>
        <Grid item>
          <Button disabled={loading} variant="contained" onClick={savePage}>
            Uložit a publikovat
            {loading ? <CircularProgress size={20} /> : null}
          </Button>
        </Grid>
        <Grid item>
          <Button disabled={loading} variant="contained" onClick={selectHistory}>
            Zobrazit historii
          </Button>
        </Grid>
      </Grid>;
      break;

    case 'history':
      toolbar = <React.Fragment></React.Fragment>;
      break;
  }

  return <Grid container wrap='nowrap'>
    <Grid item style={{ padding: '2rem' }}>
      {toolbar}
      <Typography variant="h4">Všechny stránky</Typography>
      <List>
        {data?.allPages?.nodes.map((p) => (
          <ListItem button key={p.id} onClick={() => selectPage(p)}>
            <ListItemText primary={p.url} />
          </ListItem>
        ))}
        <ListItem button onClick={startPage}>
          <ListItemIcon><AddIcon /></ListItemIcon>
          <ListItemText primary="Nová stránka" />
        </ListItem>
      </List>
    </Grid>
    <Grid item style={{ flexGrow: 1 }}>
      <ReactPage
        readOnly={state.state === 'history' || state.state === 'empty'}
        value={state.content} onChange={setContent}
      />
    </Grid>
  </Grid>;
};
