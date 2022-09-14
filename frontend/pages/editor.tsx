import * as React from 'react';
import { createValue, Value } from '@react-page/editor';
import {
  CircularProgress, Grid, TextField, List, ListItem, ListItemText, Typography, Button, ListItemIcon,
} from '@mui/material';
import { useSnackbar } from 'notistack';
import { useConfirm } from 'material-ui-confirm';
import { $, Selector, PagesOrderBy, ModelTypes } from 'lib/zeus';
import { HeadingPlugin } from 'components/Heading';
import { ContainerPlugin } from 'components/Container';
import { CallToActionPlugin } from 'components/CallToAction';
import { ReactPage, cellPlugins } from 'components/ReactPage';
import AddIcon from '@mui/icons-material/Add';
import { useTypedMutation, useTypedQuery } from 'lib/query';

type Page = ModelTypes['Page'];
type PageRevision = ModelTypes['PageRevision'];

const INITIAL_VALUE: Value = createValue({
  rows: [
    [{ plugin: HeadingPlugin }],
    [{ plugin: ContainerPlugin }],
    [{ plugin: CallToActionPlugin }],
  ],
}, {
  cellPlugins,
  lang: 'default',
});

type State = {
  state: 'empty';
  content?: Value;
} | {
  state: 'create';
  title: string;
  url: string;
  content?: Value;
} | {
  state: 'edit';
  page: Page;
  content?: Value;
} | {
  state: 'history';
  page: Page;
  current?: PageRevision;
  content?: Value;
};

const PageFragment = Selector('Page')({
  __typename: true,
  nodeId: true,
  id: true,
  url: true,
  title: true,
  content: true,
  createdAt: true,
  updatedAt: true,
});

export const EditorPage = ({ }) => {
  const { enqueueSnackbar } = useSnackbar();
  const confirm = useConfirm();
  const [loading, setLoading] = React.useState<boolean>(false);
  const [state, setState] = React.useState<State>({ state: 'empty' });
  const startPage = () => setState({ state: 'create', url: '', title: '', content: INITIAL_VALUE });
  const selectPage = (page: Page) => setState({ state: 'edit', page, content: page.content })
  const setContent = (content: Value) => setState(s => ({ ...s, content }));

  const { data, refetch } = useTypedQuery(['pages'], {
    pages: [
      { orderBy: [PagesOrderBy.URL_ASC] },
      { nodes: PageFragment },
    ],
  });

  const selectedPage = (state.state === 'edit' || state.state === 'history') ? state.page : undefined
  const { data: revisions } = useTypedQuery(['pageRevisions', selectedPage?.id], {
    pageRevisions: [
      { condition: { id: $('id', 'BigInt!') }, },
      {
        nodes: {
          __typename: true,
          nodeId: true,
          revNumber: true,
          revOperation: true,
          revTimestamp: true,
          id: true,
          url: true,
          title: true,
          content: true,
          createdAt: true,
          updatedAt: true,
        },
      },
    ],
  }, { enabled: !!selectedPage?.id }, { variables: { id: selectedPage?.id } });

  const { mutateAsync: doCreatePage } = useTypedMutation(['createPage'], {
    createPage: [
      {
        input: {
          page: {
            url: $('url', 'String!'),
            title: $('title', 'String!'),
            content: $('content', 'JSON!'),
          }
        }
      },
      { page: PageFragment },
    ],
  });

  const { mutateAsync: doSavePage } = useTypedMutation(['updatePage'], {
    updatePage: [
      {
        input: {
          id: $('id', 'BigInt!'),
          patch: {
            url: $('url', 'String!'),
            title: $('title', 'String!'),
            content: $('content', 'JSON!'),
          }
        }
      },
      { __typename: true },
    ],
  }, {
    onSuccess: () => refetch(),
  });


  let toolbar: JSX.Element | null = null;
  switch (state.state) {
    case 'create':
      const createPage = async () => {
        await confirm({
          description: `Opravdu chcete vytvořit stránku s URL ${state.url}?`,
        });
        setLoading(true);
        const data = await doCreatePage({
          variables: {
            url: state.url,
            title: state.title,
            content: state.content,
          },
        });
        await refetch();
        setLoading(false);
        setState({
          state: 'edit',
          page: data?.createPage?.page,
          content: state.content,
        } as State);
      };
      toolbar = <Grid container direction="column" spacing={2} style={{ marginBottom: '1rem' }}>
        <Grid item><Typography variant="h4">Nová stránka</Typography></Grid>
        <Grid item>
          <TextField value={state.title} placeholder="Název stránky" onChange={(e) => setState({
            ...state, title: e.target.value,
          })} />
        </Grid>
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
      const selectHistory = () => setState({ state: 'history', page: state.page });
      const savePage = async () => {
        setLoading(true);
        const { id, url } = state.page;
        await doSavePage({ variables: { id, url, content: state.content } });
        enqueueSnackbar('Stránka upravena', { variant: 'success' });
        await refetch();
        setLoading(false);
      };
      toolbar = <Grid container direction="column" spacing={2} style={{ marginBottom: '1rem' }}>
        <Grid item><Typography variant="h4">Upravit stránku</Typography></Grid>
        <Grid item>
          <TextField value={state.page.title} placeholder="Název stránky" onChange={(e) => setState({
            ...state,
            page: { ...state.page, title: e.target.value },
          })} />
        </Grid>
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
        {data?.pages?.nodes.map((p) => (
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
    <Grid item style={{ borderLeft: '1px solid black', flexGrow: 1 }}>
      <ReactPage
        readOnly={state.state === 'history' || state.state === 'empty'}
        value={state.content} onChange={setContent}
      />
    </Grid>
  </Grid>;
};

export default EditorPage;
