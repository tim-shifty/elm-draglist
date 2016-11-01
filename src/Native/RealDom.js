var _draglist$draglist$Native_RealDom = (function () {
  var makeList = function(xs, mapper) {
    if (typeof mapper === 'undefined') {
      mapper = function(x) { return x; }
    }
    var r = { ctor: 'Nil' };
    for (var i = xs.length - 1; 0 <= i; --i) {
      var v = mapper(xs[i]);
      r = { ctor: 'Cons', _0: v, _1: r };
    }
    return r;
  }

  return {
    'render': Elm.ElmRuntime.Render.Element.render,
    'query': F2(function(string, domNode) {
      return makeList(domNode.querySelectorAll());
    }),
    'textContent': function(domNode) {
      return domNode.textContent;
    },
    'className': function(domNode) {
      return domNode.className;
    },
    'attributes': function(domNode) {
      return makeList(domNode.attributes, function(a) {
        return { _0: a.name, _1: a.value };
      });
    }
  };
})();
