// Collapsing a block hides its children with `display: none`, which leaves the
// media inside them playing. Until folding drops the subtree from the editor
// outright, the next best thing is to stop it the moment it goes out of sight.
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

// The toggle sits inside the block's content; the children it hides are the
// block group beside it.
function pauseWithin(wrapper: HTMLElement) {
  const hidden = wrapper.closest('.bn-block')?.querySelector(':scope > .bn-block-group');
  for (const media of hidden?.querySelectorAll('video, audio') ?? []) {
    (media as HTMLMediaElement).pause();
  }
}
