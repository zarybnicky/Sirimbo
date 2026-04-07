import { TypedEventTarget } from 'typescript-event-target';

export type ClientPoint = {
  x: number;
  y: number;
  clientX: number;
  clientY: number;
};

type BoxSize = {
  left: number;
  right: number;
  top: number;
  bottom: number;
};

export type Bounds = {
  left: number;
  right: number;
  top: number;
  bottom: number;
  x: number;
  y: number;
};

const addEventListener: <K extends keyof WindowEventMap>(
  type: K,
  listener: (ev: WindowEventMap[K]) => any,
  options?: boolean | AddEventListenerOptions,
) => () => void = (...args) => {
  globalThis.addEventListener(args[0], args[1], args[2]);
  return () => globalThis.removeEventListener(args[0], args[1]);
};

export function isEvent(node: HTMLElement, { clientX, clientY }: ClientPoint) {
  const target = document.elementFromPoint(clientX, clientY);
  const event = target?.closest('.rbc-event');
  return !!event && node.contains(event);
}

export function pointInColumn(bounds: BoxSize, point: { x: number; y: number }) {
  const { left, right, top } = bounds;
  const { x, y } = point;
  return x < right + 1 && x > left && y > top;
}

export function getSlotAtX(rowBox: BoxSize, x: number, slots: number) {
  const cellWidth = (rowBox.right - rowBox.left) / slots;
  return Math.floor((x - rowBox.left) / cellWidth);
}

export function pointInBox(box: BoxSize, { x, y }: { x: number; y: number }) {
  return y >= box.top && y <= box.bottom && x >= box.left && x <= box.right;
}

const isTouchEvent = (e: any): e is TouchEvent => 'touches' in e && e.touches.length > 0;
function getEventCoordinates(e: TouchEvent | DragEvent | MouseEvent) {
  const target = isTouchEvent(e) ? e.touches[0]! : e;
  return {
    clientX: target.clientX,
    clientY: target.clientY,
    pageX: target.pageX,
    pageY: target.pageY,
  };
}

const clickTolerance = 10;
const longPressDurationMs = 250;

interface EventMap {
  reset: CustomEvent<void>;
  click: CustomEvent<ClientPoint>;
  select: CustomEvent<Bounds>;
  selectStart: CustomEvent<ClientPoint & { isTouch: boolean }>;
  selecting: CustomEvent<Bounds>;
  dropFromOutside: CustomEvent<ClientPoint>;
  dragOverFromOutside: CustomEvent<ClientPoint>;
}

class Selection extends TypedEventTarget<EventMap> {
  public selecting = false;
  public isDetached = false;
  public selectRect?: Bounds;
  public removeEndListener?: () => void;
  public onEscListener?: () => void;
  public removeMoveListener?: () => void;
  public removeSelectStartListener?: () => void;
  public removeLongPressListener?: () => void;
  public initialEventData?: ClientPoint & { isTouch: boolean };

  constructor(
    public container: () => HTMLElement | null,
    public options: {
      validContainers?: string[];
      shouldSelect: (point: ClientPoint) => boolean;
    } = { shouldSelect: () => true },
  ) {
    super();
    registerSelection(this);
  }

  teardown() {
    this.isDetached = true;
    unregisterSelection(this);
    this.removeEndListener?.();
    this.onEscListener?.();
    this.removeMoveListener?.();
    this.removeSelectStartListener?.();
    this.removeLongPressListener?.();
  }

  isSelected(node: HTMLElement) {
    const box = this.selectRect;
    if (!box || !this.selecting) return false;
    return objectsCollide(box, node);
  }

  startLongPressSelection(initialEvent: TouchEvent) {
    let timer: ReturnType<typeof setTimeout> | undefined;
    let removeTouchMoveListener: undefined | (() => void);
    let removeTouchEndListener: undefined | (() => void);

    const cleanup = () => {
      if (timer) {
        clearTimeout(timer);
        timer = undefined;
      }
      removeTouchMoveListener?.();
      removeTouchMoveListener = undefined;
      removeTouchEndListener?.();
      removeTouchEndListener = undefined;
      if (this.removeLongPressListener === cleanup) {
        this.removeLongPressListener = undefined;
      }
    };

    timer = setTimeout(() => {
      cleanup();
      this.handleInitialEvent(initialEvent);
    }, longPressDurationMs);
    removeTouchMoveListener = addEventListener('touchmove', cleanup);
    removeTouchEndListener = addEventListener('touchend', cleanup);
    return cleanup;
  }

  getInitialEventData(e: MouseEvent | TouchEvent) {
    const { clientX, clientY, pageX, pageY } = getEventCoordinates(e);

    const node = this.container();
    if (!node || this.isDetached) {
      return null;
    }
    if (
      e.which === 3 ||
      (e as MouseEvent).button === 2 ||
      !node.contains(document.elementFromPoint(clientX, clientY))
    ) {
      return null;
    }
    if (
      !node.contains(e.target as Node | null) &&
      !objectsCollide(node, { top: pageY, left: pageX, bottom: pageY, right: pageX })
    ) {
      return null;
    }

    const initialEventData = {
      isTouch: e.type.startsWith('touch'),
      x: pageX,
      y: pageY,
      clientX,
      clientY,
    };
    if (!this.options.shouldSelect(initialEventData)) {
      return null;
    }

    return initialEventData;
  }

  handleInitialTouchEvent(e: TouchEvent) {
    if (this.initialEventData || !this.getInitialEventData(e)) {
      return;
    }
    this.removeLongPressListener?.();
    this.removeLongPressListener = this.startLongPressSelection(e);
  }

  dropFromOutsideListener(e: DragEvent) {
    const { pageX, pageY, clientX, clientY } = getEventCoordinates(e);
    const detail = {
      x: pageX,
      y: pageY,
      clientX: clientX,
      clientY: clientY,
    };
    this.dispatchTypedEvent(
      'dropFromOutside',
      new CustomEvent('dropFromOutside', { detail }),
    );
  }

  dragOverFromOutsideListener(e: DragEvent) {
    const { pageX, pageY, clientX, clientY } = getEventCoordinates(e);
    const detail = {
      x: pageX,
      y: pageY,
      clientX: clientX,
      clientY: clientY,
    };
    this.dispatchTypedEvent(
      'dragOverFromOutside',
      new CustomEvent('dragOverFromOutside', { detail }),
    );
  }

  handleInitialEvent(e: MouseEvent | TouchEvent) {
    if (this.initialEventData) {
      return;
    }
    const initialEventData = this.getInitialEventData(e);
    if (!initialEventData) {
      return;
    }
    this.initialEventData = initialEventData;

    switch (e.type) {
      case 'mousedown':
        this.removeEndListener = addEventListener(
          'mouseup',
          this.handleTerminatingEvent.bind(this),
        );
        this.onEscListener = addEventListener(
          'keydown',
          this.handleTerminatingEvent.bind(this),
        );
        this.removeMoveListener = addEventListener(
          'mousemove',
          this.handleMoveEvent.bind(this),
        );
        this.removeSelectStartListener = addEventListener('selectstart', (e) =>
          e.preventDefault(),
        );
        break;
      case 'touchstart':
        this.container()?.style.setProperty('touch-action', 'none');
        this.handleMoveEvent(e);
        this.removeEndListener = addEventListener(
          'touchend',
          this.handleTerminatingEvent.bind(this),
        );
        this.removeMoveListener = addEventListener(
          'touchmove',
          this.handleMoveEvent.bind(this),
        );
        this.removeSelectStartListener = addEventListener('selectstart', (e) =>
          e.preventDefault(),
        );
        break;
    }
  }

  // Check whether provided event target element
  // - is contained within a valid container
  isWithinValidContainer(e: MouseEvent | TouchEvent | KeyboardEvent) {
    if (!this.options.validContainers?.length || !e.target) {
      return true;
    }
    return this.options.validContainers.some(
      (target) => !!(e.target as HTMLElement)?.closest(target),
    );
  }

  handleTerminatingEvent(e: MouseEvent | TouchEvent | KeyboardEvent) {
    this.selecting = false;

    this.container()?.style.removeProperty('touch-action');
    this.removeEndListener?.();
    this.removeMoveListener?.();
    this.removeSelectStartListener?.();

    if (!this.initialEventData) return;

    const node = this.container();
    const inRoot = !node || node.contains(e.target as Node | null);

    if ((e as KeyboardEvent).key === 'Escape' || !this.isWithinValidContainer(e)) {
      this.initialEventData = undefined;
      return setTimeout(() => this.dispatchTypedEvent('reset', new CustomEvent('reset')));
    }

    const { pageX, pageY } = getEventCoordinates(e as MouseEvent);
    const click = this.isClick(pageX, pageY);

    this.initialEventData = undefined;

    if (click && inRoot) {
      const { pageX, pageY, clientX, clientY } = getEventCoordinates(e as MouseEvent);
      const detail = { x: pageX, y: pageY, clientX, clientY };
      return setTimeout(() =>
        this.dispatchTypedEvent('click', new CustomEvent('click', { detail })),
      );
    }

    // User drag-clicked in the Selectable area
    if (!click) {
      return setTimeout(() =>
        this.dispatchTypedEvent(
          'select',
          new CustomEvent('select', { detail: this.selectRect }),
        ),
      );
    }
    return setTimeout(() => this.dispatchTypedEvent('reset', new CustomEvent('reset')));
  }

  handleMoveEvent(e: TouchEvent | MouseEvent) {
    if (!this.initialEventData || this.isDetached) {
      return;
    }
    if (e.cancelable) {
      e.preventDefault();
    }

    const { x = 0, y = 0 } = this.initialEventData || {};
    const { pageX, pageY } = getEventCoordinates(e);
    const w = Math.abs(x - pageX);
    const h = Math.abs(y - pageY);
    const left = Math.min(pageX, x);
    const top = Math.min(pageY, y);

    // Prevent emitting selectStart event until mouse is moved.
    // in Chrome on Windows, mouseMove event may be fired just after mouseDown event.
    if (this.isClick(pageX, pageY) && !this.selecting) {
      return;
    }

    this.selectRect = {
      top,
      left,
      x: pageX,
      y: pageY,
      right: left + w,
      bottom: top + h,
    };

    if (!this.selecting) {
      this.dispatchTypedEvent(
        'selectStart',
        new CustomEvent('selectStart', { detail: this.initialEventData }),
      );
      this.selecting = true;
    }
    if (!this.isClick(pageX, pageY)) {
      this.dispatchTypedEvent(
        'selecting',
        new CustomEvent('selecting', { detail: this.selectRect }),
      );
    }
  }

  isClick(pageX: number, pageY: number) {
    const { x = 0, y = 0, isTouch } = this.initialEventData || {};
    return (
      !isTouch &&
      Math.abs(pageX - x) <= clickTolerance &&
      Math.abs(pageY - y) <= clickTolerance
    );
  }
}

const registeredSelections = new Set<Selection>();
let removeMouseDownListener: undefined | (() => void);
let removeTouchStartListener: undefined | (() => void);
let removeDropListener: undefined | (() => void);
let removeDragOverListener: undefined | (() => void);

function forEachSelection(callback: (selection: Selection) => void) {
  for (const selection of registeredSelections) {
    callback(selection);
  }
}

function ensureSelectionListeners() {
  if (removeMouseDownListener) {
    return;
  }

  removeMouseDownListener = addEventListener('mousedown', (e) => {
    forEachSelection((selection) => selection.handleInitialEvent(e));
  });
  removeTouchStartListener = addEventListener('touchstart', (e) => {
    forEachSelection((selection) => selection.handleInitialTouchEvent(e));
  });
  removeDropListener = addEventListener('drop', (e) => {
    e.preventDefault();
    forEachSelection((selection) => selection.dropFromOutsideListener(e));
  });
  removeDragOverListener = addEventListener('dragover', (e) => {
    e.preventDefault();
    forEachSelection((selection) => selection.dragOverFromOutsideListener(e));
  });
}

function teardownSelectionListeners() {
  removeMouseDownListener?.();
  removeTouchStartListener?.();
  removeDropListener?.();
  removeDragOverListener?.();

  removeMouseDownListener = undefined;
  removeTouchStartListener = undefined;
  removeDropListener = undefined;
  removeDragOverListener = undefined;
}

function registerSelection(selection: Selection) {
  registeredSelections.add(selection);
  ensureSelectionListeners();
}

function unregisterSelection(selection: Selection) {
  registeredSelections.delete(selection);
  if (registeredSelections.size === 0) {
    teardownSelectionListeners();
  }
}

function objectsCollide(
  nodeA: HTMLElement | BoxSize,
  nodeB: HTMLElement | BoxSize,
  tolerance = 0,
) {
  const {
    top: aTop,
    left: aLeft,
    right: aRight = aLeft,
    bottom: aBottom = aTop,
  } = getBoundsForNode(nodeA);
  const {
    top: bTop,
    left: bLeft,
    right: bRight = bLeft,
    bottom: bBottom = bTop,
  } = getBoundsForNode(nodeB);

  return (
    aBottom - tolerance >= bTop &&
    // 'a' top doesn't touch 'b' bottom
    aTop + tolerance <= bBottom &&
    // 'a' right doesn't touch 'b' left
    aRight - tolerance >= bLeft &&
    // 'a' left doesn't touch 'b' right
    aLeft + tolerance <= bRight
  );
}

const isElement = (x: unknown): x is HTMLElement =>
  !!(x as Element)?.getBoundingClientRect;

export function getBoundsForNode(node: HTMLElement | BoxSize): BoxSize {
  if (!isElement(node)) return node as BoxSize;

  const rect = node.getBoundingClientRect();
  const left = rect.left + (window.scrollX || document.body.scrollLeft || 0);
  const top = rect.top + (window.scrollY || document.body.scrollTop || 0);

  return {
    top,
    left,
    right: (node.offsetWidth || 0) + left,
    bottom: (node.offsetHeight || 0) + top,
  };
}

export default Selection;
