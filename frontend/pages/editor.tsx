import * as React from 'react';
import { createValue, Value } from '@react-page/editor';
import {
  CircularProgress, Grid, TextField, List, ListItem, ListItemText, Typography, Button, ListItemIcon,
} from '@mui/material';
import { useSnackbar } from 'notistack';
import { useConfirm } from 'material-ui-confirm';
import { HeadingPlugin } from 'components/Heading';
import { ContainerPlugin } from 'components/Container';
import { CallToActionPlugin } from 'components/CallToAction';
import { ReactPage, cellPlugins } from 'components/ReactPage';
import AddIcon from '@mui/icons-material/Add';
import { Page, PageRevision, useCreatePageMutation, usePageListQuery, usePageRevisionsQuery, useUpdatePageMutation } from 'index';

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

export const EditorPage = ({ }) => {
  const { enqueueSnackbar } = useSnackbar();
  const confirm = useConfirm();
  const [loading, setLoading] = React.useState<boolean>(false);
  const [state, setState] = React.useState<State>({ state: 'empty' });
  const startPage = () => setState({ state: 'create', url: '', title: '', content: INITIAL_VALUE });
  const selectPage = (page: Page) => setState({
    state: 'edit',
    page,
    content: page.content as any,
  })
  const setContent = (content: Value) => setState(s => ({ ...s, content }));

  const selectedPage = (state.state === 'edit' || state.state === 'history') ? state.page : undefined
  const { data: revisions } = usePageRevisionsQuery({
    id: selectedPage?.id!
  }, {
    enabled: !!selectedPage?.id,
  });

  const { data, refetch } = usePageListQuery();
  const { mutateAsync: doCreatePage } = useCreatePageMutation();
  const { mutateAsync: doSavePage } = useUpdatePageMutation({
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
          input: {
            url: state.url,
            title: state.title,
            content: state.content || {},
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
        await doSavePage({ id, patch: { url, content: state.content } });
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
      toolbar = <></>;
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
