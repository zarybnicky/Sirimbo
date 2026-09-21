// Collapsing a block hides its children with `display: none`, which leaves the
// media inside them playing. Until folding drops the subtree from the editor
// outright, the next best thing is to stop it the moment it goes out of sight.
//
// This watches BlockNote's own toggle blocks. A fold of our own calls pauseWithin
// directly: by the time an observer callback runs, ProseMirror may already have
// replaced the node view, leaving the mutation's target detached and its block
// unreachable.
export function pauseHiddenMedia(root: HTMLElement): () => void {
  const observer = new MutationObserver((mutations) => {
    for (const { target } of mutations) {
      const wrapper = target as HTMLElement;
      if (wrapper.dataset.showChildren === 'false') {
        pauseWithin(wrapper);
      }
    }
  });

  observer.observe(root, {
    subtree: true,
    attributes: true,
    attributeFilter: ['data-show-children'],
  });

  return () => observer.disconnect();
}

// The control sits inside the block's content; the children it hides are the
// block group beside it. Called straight from a fold, and from the observer for
// BlockNote's own toggle blocks, whose control is not ours to hook.
export function pauseWithin(wrapper: HTMLElement) {
  const hidden = wrapper.closest('.bn-block')?.querySelector(':scope > .bn-block-group');
  if (!hidden) {
    return;
  }

  for (const media of hidden.querySelectorAll('video, audio')) {
    (media as HTMLMediaElement).pause();
  }

  // An embed is someone else's document, so it is asked rather than told. The
  // player keeps its position, which tearing the iframe down would lose.
  for (const frame of hidden.querySelectorAll('iframe')) {
    frame.contentWindow?.postMessage(
      JSON.stringify({ event: 'command', func: 'pauseVideo', args: '' }),
      '*',
    );
  }
}
