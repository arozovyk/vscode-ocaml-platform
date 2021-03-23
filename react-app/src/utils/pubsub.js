const subscribers = {};
const vscode = acquireVsCodeApi();

export function subscribe(topic, handler) {
  let handlers = subscribers[topic];
  if (!handlers) {
    handlers = subscribers[topic] = [];
  }
  if (handlers.indexOf(handler) === -1) {
    handlers.push(handler);
  }

  return () => handlers.splice(handlers.indexOf(handler), 1);
}

export function publish(topic, data) {
  if (!(data.range === null)) {
    vscode.postMessage({
      begin: data.range[0].toString(),
      end: data.range[1].toString()
    });
  }
  if (subscribers[topic]) {
    setTimeout(function callSubscribers() {
      if (subscribers[topic]) {
        const handlers = subscribers[topic];
        for (var i = 0; i < handlers.length; i++) {
          handlers[i](data);
        }
      }
    }, 0);
  }
}

export function clear(unsubscribers) {
  unsubscribers.forEach(call);
}

function call(f) {
  return f();
}
