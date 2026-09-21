import type { BlockSpec, PropSchema } from '@blocknote/core';
import type { FoldStore } from './folding.ts';
import { pauseWithin } from './pause-hidden-media.ts';

// Wrapping the stock list specs rather than reimplementing them: only `render`
// changes, so the parse rules, the external HTML and the Enter / Mod-Shift-8
// shortcuts all come along untouched.
//
// A spec's `render` is already composed — it calls the block's own render and
// then wraps the result in the `bn-block-content` div that carries the block
// type. The fold control has to go *inside* that div, which is where BlockNote
// puts its own toggle: the list markers are drawn by rules that want
// `.bn-block > .bn-block-content` as a direct child, so wrapping it instead of
// its contents silently costs every bullet and number its marker.
export function foldable<Name extends string, Props extends PropSchema>(
  spec: BlockSpec<Name, Props, 'inline'>,
  store: FoldStore,
): BlockSpec<Name, Props, 'inline'> {
  const { render, ...implementation } = spec.implementation;

  return {
    ...spec,
    implementation: {
      ...implementation,
      // Not an arrow: the composed render reads `renderType` and the block's DOM
      // attributes off its `this`, so the call has to forward it.
      render(...args: unknown[]) {
        const block = args[0] as { id: string };
        // eslint-disable-next-line unicorn/no-this-outside-of-class -- forwarded, see above
        const rendered = (render as (...args: unknown[]) => Rendered).apply(this, args);

        const fold = document.createElement('div');
        fold.className = 'outline-foldable';
        fold.append(control(block.id, store, fold), ...rendered.dom.childNodes);
        rendered.dom.append(fold);
        setFolded(fold, store.isFolded(block.id));

        // `dom` is left alone: it is the bn-block-content div, and everything
        // above it is BlockNote's to arrange. `contentDOM` still points at the
        // same element, one level deeper than before.
        return rendered;
      },
    },
  };
}

type Rendered = { dom: HTMLElement; contentDOM?: HTMLElement };

function setFolded(fold: HTMLElement, folded: boolean) {
  fold.dataset.folded = folded ? 'true' : 'false';
  fold.querySelector('button')?.setAttribute('aria-expanded', folded ? 'false' : 'true');
}

function control(id: string, store: FoldStore, fold: HTMLElement) {
  const button = document.createElement('button');
  button.className = 'outline-fold';
  button.type = 'button';
  button.contentEditable = 'false';
  button.tabIndex = -1;
  // Keeping the caret where it was: the click is about the tree, not the text.
  button.addEventListener('mousedown', (event) => event.preventDefault());
  button.addEventListener('click', () => {
    const folded = fold.dataset.folded !== 'true';
    store.setFolded(id, folded);
    setFolded(fold, folded);
    if (folded) {
      // While the element is still in the tree: hiding it leaves anything
      // playing inside it audible.
      pauseWithin(fold);
    }
  });
  return button;
}
