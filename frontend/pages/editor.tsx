import * as React from 'react';
import { createValue, Value } from '@react-page/editor';
import { useConfirm } from 'components/Confirm';
import { HeadingPlugin } from 'components/Heading';
import { ContainerPlugin } from 'components/Container';
import { CallToActionPlugin } from 'components/CallToAction';
import { ReactPage, cellPlugins } from 'components/ReactPage';
import { Plus as AddIcon } from 'react-feather';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { toast } from 'react-toastify';
import classNames from 'classnames';
import { TextField } from 'components/TextField';
import { Spinner } from 'components/Spinner';
import { Button } from 'components/Button';
import { Page, PageRevision } from 'lib/graphql';
import { useCreatePageMutation, usePageListQuery, usePageRevisionsQuery, useUpdatePageMutation } from 'lib/graphql/Page';

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

export default function EditorPage() {
  useRequireUserLoggedIn();
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
      toolbar = <div className="grid gap-4 mb-4">
        <h4 className="text-lg font-bold mb-2">Nová stránka</h4>
        <TextField value={state.title} placeholder="Název stránky" onChange={(e) => setState({
          ...state, title: e.currentTarget.value,
        })} />
        <TextField value={state.url} placeholder="URL stránky" onChange={(e) => setState({
          ...state, url: e.currentTarget.value,
        })} />
        <Button disabled={loading} onClick={createPage}>
          Vytvořit a publikovat
          {loading ? <Spinner /> : null}
        </Button>
      </div>;
      break;

    case 'edit':
      const selectHistory = () => setState({ state: 'history', page: state.page });
      const savePage = async () => {
        setLoading(true);
        const { id, url } = state.page;
        await doSavePage({ id, patch: { url, content: state.content } });
        toast.success('Stránka upravena');
        await refetch();
        setLoading(false);
      };
      toolbar = <div className="grid gap-4 mb-4">
        <h4 className="text-lg font-bold mb-2">Upravit stránku</h4>
        <TextField value={state.page.title} placeholder="Název stránky" onChange={(e) => setState({
          ...state,
          page: { ...state.page, title: e.currentTarget.value },
        })} />
        <TextField value={state.page.url} placeholder="URL stránky" onChange={(e) => setState({
          ...state,
          page: { ...state.page, url: e.currentTarget.value },
        })} />
        <Button disabled={loading} onClick={savePage}>
          Uložit a publikovat
          {loading ? <Spinner /> : null}
        </Button>
        <Button disabled={loading} onClick={selectHistory}>
          Zobrazit historii
        </Button>
      </div>;
      break;

    case 'history':
      toolbar = <></>;
      break;
  }

  return <div className="flex flex-nowrap">
    {toolbar}
    <h4 className="text-lg font-bold mb-2">Všechny stránky</h4>
    <div className="list-none grid gap-0.5">
      {data?.pages?.nodes.map((p) => (
        <div key={p.id} onClick={() => selectPage(p)} className={classNames(
          "p-3 hover:bg-stone-500 hover:text-white cursor-pointer",
          selectedPage?.id === p.id && 'text-white bg-stone-700',
        )}>
          {p.url}
        </div>
      ))}
      <div onClick={startPage} className={classNames(
        "p-3 hover:bg-stone-500 hover:text-white cursor-pointer",
        selectedPage === undefined && 'text-white bg-stone-700',
      )}>
        <AddIcon className="h-4.5 w-4.5 mr-3" />
        Nová stránka
      </div>
    </div>
    <div className="border-l border-black grow">
      <ReactPage
        readOnly={state.state === 'history' || state.state === 'empty'}
        value={state.content} onChange={setContent}
      />
    </div>
  </div>;
};
